# Custom functions for project

# function to mask variable
# TODO: Here and elsewhere where multiple datasets share panelists - make sure panelists are consistently renamed

mask_var <- function(df,
                     var,
                     var_levels = NULL,
                     seed = 12345) {
  set.seed(seed)
  
  # allow for specification of factor levels, in case we have multiple datasets
  # with the factor levels in common and we want to preserve the full set of levels
  # across datasets
  if (is.null(var_levels))
    var_levels <- df[[var]] %>% as.factor() %>% levels()
  
  output <- df %>%
    mutate(!!var := (!!sym(var) %>% as.character %>% factor(levels = var_levels)))
  
  # shuffle factor levels
  levels(output[[var]]) <- seq_along(levels(output[[var]])) %>%
    sample()
  
  # erase previous order information and initialize new factor variable
  output <- output %>%
    mutate(!!var := (!!sym(var) %>% as.character() %>% as.numeric() %>% as.factor()))
  
  return(output)
  
}


# read file depending on extension (csv or excel)

read_csv_or_excel <-
  function(input_file_path,
           sheet = 1,
           inside_list = TRUE) {
    file_label <- input_file_path %>%
      basename() %>%
      tools::file_path_sans_ext()
    
    ext <- input_file_path %>%
      tools::file_ext()
    
    if (str_detect(ext, "csv")) {
      # csv file
      
      output <- input_file_path %>%
        read_csv() %>%
        list() %>%
        set_names(file_label)
      
    } else if (str_detect(ext, "xls")) {
      # excel file
      
      sheet_names <- input_file_path %>%
        readxl::excel_sheets()
      
      output <- input_file_path %>%
        readxl::read_excel(sheet = sheet) %>%
        list() %>%
        set_names(str_c(file_label, "-", sheet_names[[sheet]]))
      
    } else
      stop("Need csv or excel file")
    
    if (!inside_list)
      output <- output[[1]]
    
    return(output)
  }

# perturb data for model testing purposes ----

perturb_num_data <-
  function(input_data,
           rand_effect = 0.1,
           rand_seed = 12345) {
    # establish range of possible values
    num_vals <- length(input_data)
    poss_range <- range(input_data)
    range_width <- poss_range %>%
      diff()
    min_val <- poss_range[[1]]
    max_val <- poss_range[[2]]
    
    # perturb and trim within range
    set.seed(rand_seed)
    output_data <-
      input_data + range_width * runif(num_vals, min = -0.5, max = 0.5) * rand_effect
    output_data <- ifelse(output_data > max_val, max_val, output_data)
    output_data <- ifelse(output_data < min_val, min_val, output_data)
    output_data <- output_data %>% round()
    
    return(output_data)
    
  }

perturb_cat_data <-
  function(input_data,
           rand_effect = 0.1,
           rand_seed = 12345) {
    # establish range of possible values
    poss_vals <- input_data %>%
      as.factor() %>%
      levels()
    num_vals <- input_data %>%
      length()
    
    # perturb data
    set.seed(rand_seed)
    rand_inds <- rbinom(num_vals, 1, prob = rand_effect) %>%
      as.logical() %>%
      which()
    num_rands <- rand_inds %>%
      length()
    
    output_data <- input_data
    output_data[rand_inds] <-
      sample(poss_vals, size = num_rands, replace = TRUE)
    
    
    return(output_data)
    
  }

perturb_model_data <-
  function(model_data,
           rand_effect_x = 0.1,
           rand_effect_y = 0.2,
           rand_effect_d = 0.2) {
    # Make perturbed data for model testing from standardized model data format
    
    perturbed_x_data <- model_data$x %>%
      mutate_at(str_c("X", 1:(ncol(.) - 3)), perturb_num_data)
    
    perturbed_y_data <- model_data$y %>%
      mutate_at(c("Y"), perturb_num_data, rand_effect = 0.2)
    
    perturbed_d_data <- model_data$d %>%
      mutate_at(str_c("D", 1:(ncol(.) - 1)), perturb_cat_data, rand_effect = 0.2)
    
    perturbed_data <-
      list(x = perturbed_x_data, d = perturbed_d_data, y = perturbed_y_data)
    
    return(perturbed_data)
    
    
  }

add_custom_theme <- function(plot_to_adjust, body_bg_col = "white", text_col = "black", font_size = 10, font_name = "Roboto", axis_labels = FALSE){
  
   output <- plot_to_adjust + 
    theme(
      axis.line = element_blank(),
      panel.background = element_blank(),
      plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
      panel.grid = element_blank(),
      legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
      legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
      legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
      axis.ticks = element_blank(),
      axis.text.y = element_text(
        size = font_size,
        family = font_name,
        color = text_col
      ),
      axis.text.x = element_text(
        size = font_size,
        family = font_name,
        color = text_col
      ),
      legend.text = element_text(
        size = font_size,
        family = font_name,
        color = text_col
      ),
      legend.title = element_blank(),
      panel.border = element_blank(),
    )
   
   if (!axis_labels){
    output <- output + 
      theme(
        axis.title = element_blank()
      )
    
   }

   return(output)
   
}

add_slide_with_title <-
  function(pptx_obj,
           title_value,
           layout_type,
           master_value) {
    pptx_obj_out <- pptx_obj %>%
      add_slide(layout = layout_type, master = master_value) %>%
      ph_with(value = title_value, location = ph_location_type(type = title_type))
    
    return(pptx_obj_out)
    
  }


add_slide_with_chart_full <-
  function(ppt_doc,
           chart_to_plot,
           slide_title,
           slide_width,
           slide_height,
           plot_width,
           plot_height,
           top_margin,
           layout_type,
           master_value) {
    left_pos <- (slide_width - plot_width) / 2
    
    ppt_doc <- ppt_doc %>%
      add_slide_with_title(
        title_value = slide_title,
        layout_type = layout_type,
        master_value = master_value
      ) %>%
      ph_with(
        dml(ggobj = chart_to_plot),
        location = ph_location(left = left_pos,
                               top = top_margin,
                               width = plot_width,
                               height = plot_height)
      )
  }

# set default values for adding a slide with a chart

add_slide_with_chart <-
  partial(
    add_slide_with_chart_full,
    slide_width = slide_width,
    slide_height = slide_height,
    plot_width = plot_width,
    plot_height = plot_height,
    top_margin = top_margin,
    layout_type = title_content_layout,
    master_value = master_value
  )

# make unordered list
make_ul <- function(tbl) {
  output <- tbl %>%
    as.data.frame()
  
  output$style <- NULL
  class(output) <- "unordered_list"
  
  return(output)
}

# format percentage
format_percent <- function(x, num_decimals = 0) {
  percent_val <-
    str_c(formatC(100 * x, format = "f", digits = num_decimals), "%")
  return(percent_val)
}

# make formatted workbok from dataframe
make_workbook <- function(df, output_file){
  
  wb <- createWorkbook()
  addWorksheet(wb, sheetName = "data")
  writeDataTable(wb, sheet = "data", df)
  setColWidths(wb, sheet = "data", cols = 1:ncol(df), widths = "auto")
  addStyle(wb, sheet = "data", createStyle(halign = "center"), rows = 1:(nrow(df)+1), cols = 1:ncol(df), gridExpand = TRUE)
  
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
}

# read all tabs of an excel file into a list
read_excel_list <- function(input_file_path) {
  
  # read sheet names
  
  sheet_names <- input_file_path %>% 
    excel_sheets()
  
  # load data
  
  excel_list <- sheet_names %>% 
    map(~read_excel(input_file_path, sheet = .)) %>% 
    set_names(sheet_names)
  
  return(excel_list)
}

# convert profiles from components to original variables
# TODO: FIX BASED ON SCALING

find_orig_profiles <- function(comp_profiles, res_pca) {
  
  x_mat <- comp_profiles %>%
    select(starts_with("X")) %>%
    as.matrix()
  
  orig_profiles <- t(t(x_mat %*% t(res_pca$rotation)) + res_pca$center) %>%
    as_tibble() %>%
    mutate(Stim = comp_profiles$Stim) %>%
    relocate(Stim)
  
  return(orig_profiles)
}

# convert profiles from original variables to components
# TODO: FIX BASED ON SCALING
find_comp_profiles <- function(orig_profiles, res_pca) {
  
  x_mat <- orig_profiles %>%
    select(starts_with("X")) %>%
    as.matrix()
  
  comp_profiles <- x_mat %>% 
    scale(center = res_pca$center, scale = FALSE) %>% 
    `%*%`(res_pca$rotation) %>% 
    as_tibble() %>%
    set_names(str_c("X", 1:ncol(.))) %>% 
    mutate(Stim = orig_profiles$Stim) %>%
    relocate(Stim)
  
  return(comp_profiles)
}

find_top_bottom <- function(df, var_to_sort, var_to_ref, num_to_find = 5){
  
  top_vals <- df %>% 
    slice_max(order_by = !!sym(var_to_sort), n = num_to_find) %>%
    select(one_of(c(var_to_ref, var_to_sort)))
  
  bottom_vals <- df %>% 
    slice_min(order_by = !!sym(var_to_sort), n = num_to_find)  %>%
    select(one_of(c(var_to_ref, var_to_sort))) %>% 
    arrange(desc(!!sym(var_to_sort)))
  
  output <- top_vals %>% 
    bind_rows(bottom_vals)
  
  return(output)
  
}

create_wb_from_list <- function(output_list, output_file_path){
  
  sheet_names <- output_list %>% 
    names()
  
  # Write results out to Excel
  wb <- createWorkbook()
  
  header_style <- createStyle(
    fontName = "Segoe UI",
    fontSize = 12,
    fontColour = "#000000",
    halign = "center",
    textDecoration = "bold"
  )
  
  body_style <-
    createStyle(
      fontName = "Segoe UI",
      fontSize = 12,
      fontColour = "#000000",
      halign = "center",
      numFmt = "#0.00"
    )
  
  for (sheet_name in sheet_names) {
    worksheet_data <- output_list[[sheet_name]]
    
    ## add worksheet
    addWorksheet(wb, sheet_name)
    
    ## write data to worksheet
    writeData(wb, sheet_name, worksheet_data, rowNames = FALSE)
    
    # define widths
    setColWidths(wb,
                 sheet_name,
                 cols = 1:ncol(worksheet_data),
                 widths = 40)
    
    # apply styles
    addStyle(
      wb,
      sheet_name,
      header_style,
      rows = 1,
      cols = 1:ncol(worksheet_data),
      gridExpand = TRUE
    )
    
    addStyle(
      wb,
      sheet_name,
      body_style,
      rows = 2:(nrow(worksheet_data) + 1),
      cols = 1:ncol(worksheet_data),
      gridExpand = TRUE
    )
    
  }
  
  # save results
  saveWorkbook(wb, file = output_file_path, overwrite = TRUE)
  
}

find_num_clusts <- function(df_wide, min_num_clusts = 4, max_num_clusts = 6){
  
  remove_vars <- df_wide %>%
    nearZeroVar()
  
  num_clust_res <- df_wide %>%
    select(-remove_vars) %>%
    scale() %>% 
    fviz_nbclust(hcut, method = "gap_stat", k.max = max_num_clusts) 
  
  num_clusts <- num_clust_res$data %>% 
    rownames_to_column("Number") %>% 
    mutate(Number = as.numeric(Number)) %>% 
    as_tibble() %>% 
    filter(Number >= min_num_clusts) %>% 
    filter(Number <= max_num_clusts) %>% 
    filter(gap == max(gap)) %>% 
    .[["Number"]] %>% 
    as.numeric()
  
  return(num_clusts)
  
}

make_cluster_list <- function(df_wide, num_clusts_k = NULL) {

  #browser()
    
  remove_vars <- df_wide %>%
    nearZeroVar()
  
  df_dist <- df_wide %>%
    #select(-remove_vars) %>%
    scale() %>%
    t() %>%
    dist()
  
  res_hc <- hclust(d = df_dist, method = "ward.D2")
  
  if (!is.null(num_clusts_k)){
    clust_inds <- cutree(res_hc, k = num_clusts_k)
  } else{
    clust_inds <- NULL
  }
  
  #browser()
  
  phylo_plot <- res_hc %>%
    fviz_dend(type = "phylogenic",
              repel = TRUE, 
              k = num_clusts_k)  +
    theme_void() +
    theme(
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
      legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
      legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
      legend.position = "bottom",
      axis.title = element_blank()
    )
  
  return(list(plot = phylo_plot, clust_inds = clust_inds))
  
}

predict_pop_liking_full <- function(profile_data, pop_data, selected_model){
  
  # create general function that computes average liking scores for a series of samples for a given population
  # this function accepts a matrix of profile means along with a Stim column used for indexing
  
  # predict stimulus means for this population
  
  input_data <- profile_data %>% 
    merge(pop_data) # note, dangerous function, be careful!
  
  input_frame <- input_data %>%
    as.h2o(destination_frame = "input")
  
  pred_res <- input_data %>%
    select(ID_Y, Stim) %>%
    mutate(Pred = h2o.predict(selected_model, input_frame) %>% as.vector())
  
  pred_stim_means <- pred_res %>%
    group_by(Stim) %>%
    summarize_at(c("Pred"), mean)
  
  return(pred_stim_means)
  
}

# convert profiles from components to original variables
# note we rely on actual names in the original profile here not on X1, etc.
find_orig_profiles <- function(comp_profiles, res_pca) {
  
  x_mat <- comp_profiles %>%
    select(starts_with("PC")) %>%
    as.matrix()
  
  orig_profiles <- t(t((x_mat %*% t(res_pca$rotation)) %*% (diag(res_pca$scale))) + res_pca$center) %>%
    as_tibble() %>%
    set_names(names(res_pca$center)) %>% 
    mutate(Stim = comp_profiles$Stim) %>%
    relocate(Stim)
  
  return(orig_profiles)
}

# convert profiles from original variables to components
# note we rely on actual names in the original profile here not on X1, etc.
find_comp_profiles <- function(orig_profiles, res_pca) {
  
  x_mat <- orig_profiles %>%
    select(one_of(names(res_pca$center))) %>% 
    as.matrix()
  
  comp_profiles <- x_mat %>% 
    scale(center = res_pca$center, scale = res_pca$scale) %>% 
    `%*%`(res_pca$rotation) %>% 
    as_tibble() %>%
    set_names(str_c("PC", 1:ncol(.))) %>% 
    mutate(Stim = orig_profiles$Stim) %>%
    relocate(Stim)
  
  return(comp_profiles)
}

# make lotus plots
make_lotus_plot <- function(lotus_data, col_pal = NULL, min_val = NULL, max_val = NULL, sort_atts = FALSE) {
  
#  browser()
  
  if(is.null(min_val)) min_val = min(lotus_data$Value)
  if(is.null(max_val)) max_val = max(lotus_data$Value)
  
  chart_data <- lotus_data %>%
    mutate_at(c("Sample", "Attribute"), factor) 
  
  if (sort_atts){
    
    chart_data <- chart_data %>% 
      mutate(Attribute = fct_reorder(Attribute, -Value))
  } 
  
  chart_data <- chart_data %>% 
    mutate(AttributeInd = as.numeric(fct_rev(Attribute)))
  
  # add zero values for AttributeInd
  
  zero_data <- chart_data %>%
    filter(AttributeInd == max(AttributeInd)) %>%
    mutate(AttributeInd = 0)
  
  label_data <- chart_data %>% 
    select(AttributeInd, Attribute) %>% 
    unique()
  
  lotus_plot <- chart_data %>%
    bind_rows(zero_data) %>%
    ggplot(aes(x = AttributeInd, y = Value, color = Sample)) +
    geom_point(size = 1.05) +
    geom_line() +
    scale_y_continuous(limits = c(min_val, max_val)) + 
    geom_label(aes(label = Attribute),
               y = 1.1*max_val,
               color = "black",
               label.size = 0,
               label.r = unit(0, "in"), 
               data = label_data) +
    coord_polar(start = -2 * pi / length(levels(chart_data$Attribute))) +
    scale_x_continuous(
      labels = levels(chart_data$Attribute),
      breaks = as.numeric(chart_data$Attribute) %>% unique() %>% sort()
    ) +
    theme(
      axis.line = element_blank(),
      panel.background = element_rect(fill = body_bg_col, color = body_bg_col),
      plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey40", linetype = "dotdash"),
      legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
      legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
      legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks.y = element_blank(),
      text = element_text(
        size = font_size,
        family = font_name,
        color = text_col
      ),
      panel.border = element_blank(),
      legend.position = "bottom"
    ) 
  
  if (!is.null(col_pal)) {
    lotus_plot <- lotus_plot +
      scale_color_manual(values = col_pal)
    
  }
  
  return(lotus_plot)
  
}

# data splitting functions

# helper functions for data splitting

.train_ID_Y <- NULL

get_train_ID_Y <- function(train_prop = 0.8, seed = 12345) {
  set.seed(seed)
  
  if (is.null(.train_ID_Y)) {
    unique_ID_Y <- unique(yyy$ID_Y)
    .train_ID_Y <<-
      sample(unique_ID_Y, round(train_prop * length(unique_ID_Y)), replace = FALSE)
  }
  
  .train_ID_Y
}

.train_rows_x <- NULL

get_train_rows_x <- function(train_prop = 1, seed = 76542){
  
  set.seed(seed)
  .train_rows_x <- sample(1:nrow(xxx), floor(nrow(xxx) * train_prop))
  
  .train_rows_x
  
}

get_train_x <- function(train_prop = 0.7, seed = 76542)
  return(xxx[get_train_rows_x(train_prop, seed), ])

get_test_x <- function(train_prop = 0.7, seed = 76542)
  return(xxx[-get_train_rows_x(train_prop, seed), ])  

get_train_d <-
  function(train_prop = 0.7, seed = 12345)
    return(ddd %>% filter(ID_Y %in% get_train_ID_Y(train_prop, seed)))

get_test_d <-
  function(train_prop = 0.7, seed = 12345)
    return(ddd %>% filter(!(
      ID_Y %in% get_train_ID_Y(train_prop, seed)
    )))

get_train_y <-
  function(train_prop = 0.7, seed = 12345)
    return(yyy %>% filter(ID_Y %in% get_train_ID_Y(train_prop, seed)))

get_test_y <-
  function(train_prop = 0.7, seed = 12345)
    return(yyy %>% filter(!(
      ID_Y %in% get_train_ID_Y(train_prop, seed)
    )))


