# Load packages for project, installing if necessary





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

packages <- c(
  "tidyverse",
  "FactoMineR",
  "factoextra",
  "rvg", 
  "ggrepel")

load_package(packages)

body_bg_col <- "white"
body_text_col <- "black"

make_mfa_biplot <-
  function(res_mfa,
           dim_vect,
           plot_prods = TRUE) {
    
    #browser()
    dim_1 <- dim_vect[[1]]
    dim_2 <- dim_vect[[2]]
    
    sample_info <- res_mfa$ind$coord[, dim_vect] %>%
      as.data.frame() %>%
      rownames_to_column(var = "Sample") %>%
      set_names(c("Sample", "x", "y"))
    
    group_names <- res_mfa$group$coord %>%
      rownames()
    
    output <- res_mfa %>%
      fviz_mfa_var(
        "quanti.var",
        repel = TRUE,
        geom = c("text"),
        axes = c(dim_1, dim_2),
        ggtheme = theme_minimal(),
        title = "",
        font.family = font_name, 
        select.var = list(contrib = 20)
      )
    
    if (plot_prods) {
      output <- output +
        geom_text_repel(aes(x = x, y = y, label = Sample),
                        data = sample_info,
                        col = "black")
      
    }
    
    output <- output +
      theme_void() +
      theme(
        text = element_text(
          family = font_name,
          color = gsk_grey,
          size = font_size / 2,
          vjust = 0.5
        ),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
        legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
        legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
        legend.position = "bottom",
        axis.title = element_blank()
      )
    
    return(output)
    
  }




make_mfa_titles <- function(dim_vect) {
  dim_1 <- dim_vect[[1]]
  dim_2 <- dim_vect[[2]]
  
  output <- str_c("Dimension ", dim_1, " vs Dimension ", dim_2)
  
  return(output)
  
}

make_imp_chart <- function(rel_axes) {
  
  #browser()
  
  dd <- res_mfa %>%
    facto_summarize(element = "quanti.var",
                    result = "contrib",
                    axes = rel_axes)
  
  contrib <- dd$contrib
  names(contrib) <- rownames(dd)
  theo_contrib <- 100 / length(contrib)
  
  if (length(rel_axes) > 1) {
    eig <- get_eigenvalue(res_mfa)[rel_axes, 1]
    theo_contrib <- sum(theo_contrib * eig) / sum(eig)
  }
  
  output <- res_mfa %>%
    fviz_contrib(choice = "quanti.var",
                 axes = rel_axes, top = 50) +
    # fill = red_m,
    # color = red_m) +
    theme(
      panel.background = element_blank(),
      plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
      panel.grid = element_blank(),
      text = element_text(
        size = font_size,
        family = font_name,
        color = body_text_col
      ),
      legend.position = "top"
    ) +
    ggtitle(label =  "") +
    geom_hline(yintercept = theo_contrib,
               color = gsk_grey,
               linetype = 2) 
  
  return(output)
  
}


make_scree_plot <- function(res_mfa) {
  scree_plot <- res_mfa %>%
    fviz_screeplot(
      addlabels = TRUE,
      barfill = gsk_blue,
      barcolor = gsk_blue,
      linecolor = gsk_red,
      main = ""
    ) +
    geom_hline(yintercept = eigenvalue_thresh,
               color = gsk_orange,
               linetype = 2) +
    theme(
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      text = element_text(
        size = font_size,
        family = font_name,
        color = "black"
      )
    )
  
  return(scree_plot)
  
}

#' create a section of slides from a plot list, starting with a section header
make_slides_plot_list <- function(pptx_obj,
                                  plot_list,
                                  section_layout,
                                  master_value,
                                  section_ph) {
  section_titles <- plot_list %>%
    names()
  
  for (section_title in section_titles) {
    pptx_obj <- pptx_obj %>%
      add_slide(layout = section_layout, master = master_value) %>%
      ph_with(value = section_title,
              location = ph_location_label(ph_label = section_ph))
    
    charts_to_plot <- plot_list[[section_title]]
    
    chart_titles <- charts_to_plot %>%
      names()
    
    for (chart_title in chart_titles) {
      chart_to_plot <- charts_to_plot[[chart_title]]
      
      pptx_obj <- pptx_obj %>%
        add_slide_with_chart(chart_to_plot, chart_title)
      
    }
    
  }
  
  return(pptx_obj)
}

