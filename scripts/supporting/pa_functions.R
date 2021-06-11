

load_package <- function(x) {
  for (i in x) {
    #  require returns TRUE invisibly if it was able to load package
    if (!require(i , character.only = TRUE)) {
      #  If package was not able to be loaded then re-install
      install.packages(i , dependencies = TRUE)
      #  Load package after installing
      require(i , character.only = TRUE)
    }
  }
}

#  Then try/install packages...

packages <-
  c(
    "tidyverse",
    "readxl",
    "officer",
    "flextable",
    "agricolae",
    "igraph",
    "PMCMRplus",
    "DescTools",
    "openxlsx"
  )

suppressWarnings(load_package(packages))

# Define constants for tutorial

# Set color palette and font choices
rgb_std <- partial(rgb, max = 255)
table_shading <- rgb_std(242, 242, 242) # light grey
sig_shading <- rgb_std(255, 255, 0) # yellow for highlighting

border_solid <- fp_border(color = "black", width = 1)

font_name <- "Arial"
font_size <- 11
small_font_size <- 10

# Set common text formatting

small_text <- fp_text(font.family = font_name, font.size = small_font_size)
normal_text <- fp_text(font.family = font_name, font.size = font_size)
bold_text <- fp_text(font.family = font_name, font.size = font_size, bold = TRUE)

# Set number of decimals for reporting percentages
num_decimals <- 1

# Set column widths (TODO: Add these to config file and read in)

# table 1

col_width_1_1 <- 2.65 
col_width_1_2 <- 2.18 
col_width_1_3 <- 0.67 

# table 2

col_width_2_1 <- 0.62
col_width_2_2 <- 2.18 
col_width_2_3 <- 1.5
col_width_2_4 <- 1.02

# table 3

col_width_3_1 <- 1.19
col_width_3_2 <- 1.23
col_width_3_3 <- 0.58
col_width_3_4 <- 0.57
col_width_3_6 <- 0.78
col_width_3_7 <- 0.94



create_penalty_analysis_table <- function(main_data,
                                          sample_name,
                                          jar_var,
                                          liking_var,
                                          jar_labels,
                                          num_decimals = 1,
                                          alpha_level = 0.05,
                                          percent_thresh = 0.2) {
  
  #browser()
  
  rel_data <- main_data %>%
    select(Sample_Name, one_of(jar_var), one_of(liking_var)) %>%
    filter(Sample_Name == sample_name) %>%
    set_names(c("Sample", "JAR", "Liking")) %>%
    mutate(JAR = factor(
      JAR,
      levels = 1:5,
      labels = rep(jar_labels, times = c(2, 1, 2))
    ))
  
  # find liking means
  liking_means <- rel_data %>%
    group_by(JAR, .drop = FALSE) %>%
    summarize(Liking = mean(Liking))
  
  # count number of subjects per JAR meta-categories
  
  count_info <- rel_data %>%
    group_by(JAR, .drop = FALSE) %>%
    tally()
  
  total_count <- count_info %>%
    pull(n) %>%
    sum()
  
  # create flags for missing categories
  
  too_weak_missing <- (count_info$n[[1]] == 0)
  too_strong_missing <- (count_info$n[[3]] == 0)
  
  # determine percentage of JAR meta-categories and append Liking information
  
  output <- count_info %>%
    mutate(total = sum(n),
           Percent = format_percent(n / total, num_decimals = num_decimals)) %>%
    select(Percent) %>%
    cbind(liking_means) %>%
    mutate(fLiking = format_decimal(Liking, num_decimals = num_decimals)) %>%
    select(JAR, Percent, Liking, fLiking)
  
  # prepare to conduct penalty analysis
  
  output$Drop <- ""
  output$pVal <- ""
  output$Sig <- ""
  
  # Run Fisher's LSD analysis on Liking as a function of JAR score
  
  # we now conduct the penalty analysis, taking care if there are missing meta-categories
  
  split_liking <- rel_data %>%
    pull(Liking) %>%
    split(rel_data$JAR)
  
  if (too_weak_missing) {
    output$fLiking[[1]] <- ""
    
    if (too_strong_missing) {
      # If all responses are just about right, we can't run the penalty analysis
      
      output$fLiking[[3]] <- ""
      
    } else {
      # too strong only
      output$Drop[[3]] <-
        format_decimal(output$Liking[[2]] - output$Liking[[3]], num_decimals = num_decimals)
      
      # conduct Fisher's LSD test
      p_res <- lsdTest(rel_data$Liking, rel_data$JAR)$p.value[[1]]
      
      # adjust output for p_res < 0.001
      output$pVal[[3]] <-
        ifelse(p_res < 0.0001,
               "< 0.0001",
               format_decimal(p_res, num_decimals = 4))
      
      # format output for significant results
      sig_res <- (p_res <= alpha_level)
      output$pVal[[3]] <- ifelse(sig_res, output$pVal[[3]], "")
      output$Sig[[3]] <- ifelse(sig_res, "Yes", "")
      
    }
    
  } else {
    if (too_strong_missing) {
      output$fLiking[[3]] <- ""
      
      # too weak only
      output$Drop[[1]] <-
        format_decimal(output$Liking[[2]] - output$Liking[[1]], num_decimals = num_decimals)
      
      # conduct Fisher's LSD test
      p_res <- lsdTest(rel_data$Liking, rel_data$JAR)$p.value[[1]]
      
      # adjust output for p_res < 0.001
      output$pVal[[1]] <-
        ifelse(p_res < 0.0001,
               "< 0.0001",
               format_decimal(p_res, num_decimals = 4))
      
      # format output for significant results
      sig_res <- (p_res <= alpha_level)
      output$pVal[[1]] <- ifelse(sig_res, output$pVal[[1]], "")
      output$Sig[[1]] <- ifelse(sig_res, "Yes", "")
      
      
    } else {
      # both too weak and too strong
      output$Drop[[1]] <-
        format_decimal(output$Liking[[2]] - output$Liking[[1]], num_decimals = num_decimals)
      output$Drop[[3]] <-
        format_decimal(output$Liking[[2]] - output$Liking[[3]], num_decimals = num_decimals)
      
      # If we have both too week and too strong, we run Fisher's LSD test on liking scores
      
      p_vals <- lsdTest(rel_data$Liking, rel_data$JAR)$p.value %>%
        diag()
      
      # adjust output for p_res < 0.001
      output$pVal[c(1, 3)] <-
        ifelse(p_vals < 0.0001,
               "< 0.0001",
               format_decimal(p_vals, num_decimals = 4))
      
      # format output for significant results
      sig_vals <- (p_vals <= alpha_level)
      
      output$pVal[c(1, 3)] <-
        ifelse(sig_vals, output$pVal[c(1, 3)], "")
      output$Sig[c(1, 3)] <- ifelse(sig_vals, "Yes", "")
      output[2, c("pVal", "Sig")] <- ""
      
    }
    

    return(output)
    
  }
  
  
  # PRESERVE THE FOLLOWING CODE AS DUNNETT'S TEST AND T-TESTS ARE MORE APPROPRIATE
  
  # # we now conduct the penalty analysis, taking care if there are missing meta-categories
  #
  # split_liking <- rel_data %>%
  #   pull(Liking) %>%
  #   split(rel_data$JAR)
  #
  # if (too_weak_missing){
  #
  #   output$fLiking[[1]] <- ""
  #
  #   if (too_strong_missing){
  #
  #     # If all responses are just about right, we can't run the penalty analysis
  #
  #     output$fLiking[[3]] <- ""
  #
  #   } else {
  #
  #     # too strong only
  #     output$Drop[[3]] <- format_decimal(output$Liking[[2]] - output$Liking[[3]], num_decimals = num_decimals)
  #
  #     # conduct one-tailed t-test
  #     p_res <- t.test(split_liking$`0`, split_liking$`1`, alternative = "greater")$p.value
  #
  #     # adjust output for p_res < 0.001
  #     output$pVal[[3]] <- ifelse(p_res < 0.0001, "< 0.0001", format_decimal(p_res, num_decimals = 4))
  #
  #     # format output for significant results
  #     sig_res <- (p_res <= alpha_level)
  #     output$pVal[[3]] <- ifelse(sig_res, output$pVal[[3]], "")
  #     output$Sig[[3]] <- ifelse(sig_res, "Yes", "")
  #
  #
  #
  #   }
  #
  # } else {
  #
  #   if (too_strong_missing){
  #
  #     output$fLiking[[3]] <- ""
  #
  #     # too weak only
  #     output$Drop[[1]] <- format_decimal(output$Liking[[2]] - output$Liking[[1]], num_decimals = num_decimals)
  #
  #     # conduct one-tailed t-test
  #     p_res <- t.test(split_liking$`0`, split_liking$`-1`, alternative = "greater")$p.value
  #
  #     # adjust output for p_res < 0.001
  #     output$pVal[[1]] <- ifelse(p_res < 0.0001, "< 0.0001", format_decimal(p_res, num_decimals = 4))
  #
  #     # format output for significant results
  #     sig_res <- (p_res <= alpha_level)
  #     output$pVal[[1]] <- ifelse(sig_res, output$pVal[[1]], "")
  #     output$Sig[[1]] <- ifelse(sig_res, "Yes", "")
  #
  #
  #   } else {
  #
  #     # both too weak and too strong
  #     output$Drop[[1]] <- format_decimal(output$Liking[[2]] - output$Liking[[1]], num_decimals = num_decimals)
  #     output$Drop[[3]] <- format_decimal(output$Liking[[2]] - output$Liking[[3]], num_decimals = num_decimals)
  #
  #     # If we have both too week and too strong, we run Dunnett's test on liking scores
  #
  #     dun_data <- rel_data %>%
  #       select(JAR, Liking) %>%
  #       mutate(JAR = factor(JAR, levels = c(0, -1, 1)))
  #
  #     dun_res <- dunnettTest(Liking ~ JAR, data = dun_data, alternative = "less")
  #     p_vals <- dun_res$p.value %>% as.numeric()
  #
  #     # adjust output for p_res < 0.001
  #     output$pVal[c(1,3)] <- ifelse(p_vals < 0.0001, "< 0.0001", format_decimal(p_vals, num_decimals = 4))
  #
  #     # format output for significant results
  #     sig_vals <- (p_vals <= alpha_level)
  #
  #     output$pVal[c(1,3)] <- ifelse(sig_vals, output$pVal[c(1,3)], "")
  #     output$Sig[c(1,3)] <- ifelse(sig_vals, "Yes", "")
  #     output[2,c("pVal", "Sig")] <- ""
  #
  #   }
  #
  # }
  
  # if percent is less than threshold, then we don't display results
  
  percent_num <- output$Percent %>%
    str_replace("%", "") %>%
    as.numeric() %>%
    (function(x)
      x / 100)
  
  output[(percent_num < percent_thresh), c("pVal", "Sig")] <- ""
  
  # clean up results
  
  output <- output %>%
    select(-Liking) %>%
    rename(Liking = fLiking)
  
  output[output == NaN] <- ""
  
  row.names(output) <- NULL
  
  return(output)
  
  
}

format_penalty_analysis_table <-
  function(split_result,
           col_width_1 = col_width_3_1,
           col_width_2 = col_width_3_2,
           col_width_3 = col_width_3_3,
           col_width_4 = col_width_3_4,
           col_width_6 = col_width_3_6,
           col_width_7 = col_width_3_7,
           bg_shading = sig_shading) {
    num_rows <- split_result %>%
      nrow()
    
    num_vars <- num_rows / 3
    
    output <- split_result %>%
      set_names(str_c("X_", 1:ncol(.))) %>%
      flextable() %>%
      merge_v(j = 1) %>%
      set_header_labels(
        X_1 = "Variable",
        X_2 = "Level",
        X_3 = "%",
        X_4 = "Mean Liking",
        X_5 = "Mean Drops",
        X_6 = "p-value",
        X_7 = "Significant"
      ) %>%
      border_remove() %>%
      align(align = "center", part = "all") %>%
      fontsize(size = font_size, part = "all") %>%
      font(fontname = font_name, part = "all") %>%
      autofit(add_w = 0, add_h = 0) %>%
      border(border.top = border_solid, part = "header") %>%
      border(i = (3 * (1:num_vars) - 2), border.top = border_solid) %>%
      border(i = num_rows, border.bottom = border_solid) %>%
      fix_border_issues() %>%
      bg(i = which(split_result$Sig == "Yes"),
         j = 2:7,
         bg = bg_shading) %>%
      align(align = "center", part = "all") %>%
      fontsize(size = font_size, part = "all") %>%
      font(fontname = font_name, part = "all") %>%
      autofit(add_w = 0, add_h = 0) %>%
      padding(padding = 0, part = "all") %>%
      width(j = 1, width = col_width_1) %>%
      width(j = 2, width = col_width_2) %>%
      width(j = 3, width = col_width_3) %>%
      width(j = 4:5, width = col_width_4) %>%
      width(j = 6, width = col_width_6) %>%
      width(j = 7, width = col_width_7)
    
    return(output)
    
  }

format_percent <- function(x, num_decimals = 0) {
  percent_val <-
    str_c(formatC(100 * x, format = "f", digits = num_decimals), "%")
  
  return(percent_val)
  
}

format_decimal <- function(x, num_decimals = 0) {
  format_val <-
    formatC(x, format = "f", digits = num_decimals)
  
  return(format_val)
  
}

