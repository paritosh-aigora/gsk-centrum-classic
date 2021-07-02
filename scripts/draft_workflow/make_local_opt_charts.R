
# load data and compute panel means

clean_data_path <-
  file.path("input",
            "cleaned_data",
            str_c(source_stem, "_cleaned_data.Rdata"))

load(file = clean_data_path)

# create look up keys

o_names_vect <- x_names_vect <- cleaned_keys$x %>%
  deframe()

x_names_vect <- cleaned_keys$x %>%
  select(var_name, var) %>%
  deframe()

# calculate panel means

panel_data <- cleaned_data$x %>%
  select(Stim, starts_with("X")) %>%
  group_by(Stim) %>%
  summarize_all(mean, na.rm = TRUE) %>%
  ungroup() %>%
  left_join(cleaned_keys$s) %>%
  select(Stim_Name, starts_with("X"))


# panel_data <- cleaned_data$x %>%
#   select(Stim, starts_with("X")) %>%
#   group_by(Stim) %>%
#   summarize_all(mean, na.rm = TRUE) %>%
#   pivot_longer(cols = starts_with("X"), names_to = "Variable", values_to = "Value") %>%
#   left_join(cleaned_keys$x, by = c("Variable" = "var")) %>%
#   left_join(panel_details, by = c("var_name"="Variable")) %>%
#   mutate(Value = (Value - Min_val) / (Max_val - Min_val)) %>%
#   select(Stim, Variable, Value) %>%
#   ungroup() %>%
#   left_join(cleaned_keys$s) %>%
#   #select(Stim_Name, starts_with("X")) %>%
#   pivot_wider(names_from = "Variable", values_from = "Value")

range_data <- panel_data %>%
  select(Stim_Name, starts_with("X")) %>%
  pivot_longer(starts_with("X"), names_to = "var", values_to = "value") %>%
  group_by(var) %>%
summarise(
  min_data = min(value),
  max_data = max(value)
) %>%
  mutate(range = max_data - min_data) %>%
  mutate(
    min_data = min_data - 0.05 * range,
    max_data = max_data + 0.05 * range
  ) %>% 
  left_join(cleaned_keys$x, by = "var") %>% 
  left_join(panel_details, by = c("var_name"="Variable")) %>%
  mutate(
    Min = pmax(Min_val, min_data),
    Max = pmin(Max_val, max_data)
  ) %>%
  mutate(Range = Max - Min) %>%
  mutate(Variable = tools::toTitleCase(var_name)) %>%
  select(Variable, Min, Max, Range)

# range_data <- cleaned_data$x %>%
#   group_by(Stim) %>%
#   summarize_all(mean, na.rm = TRUE) %>%
#   ungroup() %>%
#   select(Stim, starts_with("X")) %>%
#   pivot_longer(starts_with("X"), names_to = "var", values_to = "value") %>%
#   left_join(cleaned_keys$x, by = c("var" = "var")) %>% 
#   left_join(panel_details, by = c("var_name"="Variable")) %>% 
#   group_by(var) %>%
#   #summarize(Min = min(value), Max = max(value)) %>%
#   mutate(Max = Max_val, Min = Min_val) %>% 
#   mutate(Range = Max - Min) %>%
#   select(var, Max, Range) %>%
#   left_join(cleaned_keys$x) %>%
#   mutate(Variable = tools::toTitleCase(var_name)) %>%
#   select(Variable, Max, Range)

# load variable importance info ----

visualization_info_path <-
  file.path(
    "output",
    "modeling_results",
    source_stem,
    str_c(source_stem, "_visualization_info.RData")
  )

load(file = visualization_info_path)

local_opt_list_list <- full_dol_list

var_imp_info <- import_mat[[1]] %>%
  rename(var = Variable, rf = "Random Forest", gbt = "Gradient Boosted Trees") %>%
  mutate(value = (gbt + rf) / 2) %>%
  mutate(var = fct_reorder(factor(var), value)) %>%
  mutate(text = format_percent(value))

imp_thresh <- 0.01
num_top_vars <- 20

rel_var_imp_info <- var_imp_info %>%
  filter(Type == "Panel") %>%
  mutate(value = value / sum(value), text = format_percent(value)) %>%
  filter(value >= imp_thresh) %>%
  arrange(desc(value)) %>%
  select(var, value) %>%
  rename(Variable = var, Importance = value) %>%
  slice(1:num_top_vars) %>%
  left_join(range_data)

# create function to make list of global optimization plots for a fixed global optimum

prop_thresh <- 0.1 # for testing
alpha_val <- 0.5 # for testing

make_local_opt_chart <-
  function(local_opt,
           prop_thresh = 0.1,
           alpha_val = 0.5,
           opt_min = 0, 
           opt_max = 100) {
    
    if (nrow(local_opt) == 0){
      
      local_opt_chart <- NULL
    
      } else {
        
    
    
    local_info <- local_opt %>%
      slice(1) %>%
      select(Stim, Improvement, one_of(o_names_vect)) %>%
      set_names(c("Stim", "Improvement", x_names_vect)) %>%
      left_join(cleaned_keys$s)
    
    stim_val <- local_info$Stim_Name[[1]]
    improve_val <- local_info$Improvement[[1]]
    
    panel_row <- panel_data %>%
      filter(Stim_Name == stim_val) %>%
      select(starts_with("X")) %>%
      mutate(Type = "Value")
    
    opt_data <- local_info %>%
      select(starts_with("X")) %>%
      mutate(Type = "Optimum") %>%
      bind_rows(panel_row) %>%
      pivot_longer(starts_with("X"),
                   names_to = "var",
                   values_to = "value") %>%
      pivot_wider(names_from = Type, values_from = value) %>%
      left_join(cleaned_keys$x) %>%
      mutate(Variable = tools::toTitleCase(var_name)) %>%
      select(Variable, Optimum, Value) %>%
      inner_join(rel_var_imp_info)
    
    flag_thresh <- 0.05
    #browser()
    o_data <- opt_data %>%
      mutate(alpha = if_else(abs(Value - Optimum) >= prop_thresh * Range, 1, alpha_val)) %>%
      rename(Sample = Value) %>%
      
      mutate(Diff = abs(Sample - Optimum)/Range,
             flag = (Diff <= flag_thresh)) %>%
      mutate(Variable = fct_reorder(factor(Variable), Diff)) %>%
      pivot_longer(c(Sample, Optimum),
                   names_to = "Source",
                   values_to = "Value") %>%
      mutate(
        plot_value = (Value - Min) / Range,
        plot_label = formatC(
          Value,
          format = "f",
          width = 5,
          digits = 1
        )
      ) %>%
      select(Variable, Source, flag, plot_value, plot_label, alpha)
    
    hjust_val <- 0
    
    if(min(o_data$alpha) == 1) {
      
      temp_p <- o_data %>%
        ggplot(aes(
          x = Variable,
          y = plot_value,
          fill = Source
        ))
    } else {
      
      temp_p <- o_data %>%
        ggplot(aes(
          x = Variable,
          y = plot_value,
          fill = Source,
          alpha = alpha
        ))
      
    }
    
    prof_comp_plot_full <- temp_p +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(
        aes(
          x = Variable,
          y = plot_value,
          label = plot_label,
          hjust = hjust_val,
          vjust = 0.5
        ),
        #    fill = "white",
        color = text_col,
        size = 0.2 * font_size,
        family = font_name,
        #   label.r = unit(0, "lines"),
        position = position_dodge(width = 1)
      ) +
      coord_flip() +
      scale_x_discrete(name = "") +
      scale_y_continuous(name = "", limits = c(0, 1)) +
      scale_fill_manual(
        values = c(gsk_green2, gsk_orange),
        breaks = c("Optimum", "Sample"),
        labels = c("Local Optimum", "Sample Profile"),
        name = "",
        guide = guide_legend(reverse = TRUE)
      ) +
      theme(
        axis.line = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
        legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
        legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        legend.text = element_text(
          size = font_size,
          family = font_name,
          color = text_col
        ),
        legend.title = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill = gsk_grey)
      ) +
      guides(alpha = FALSE)
    
    prof_comp_plot_full
    #browser()
    # var_imp_plot_s_data <- var_imp_info %>%
    #   filter(Type == "Panel") %>%
    #   mutate(value = value / sum(value), text = format_percent(value)) %>%
    #   filter(value >= imp_thresh) %>%
    #   inner_join(o_data %>%  distinct(Variable, alpha), by = c("var" = "Variable")) %>%
    #   mutate(Type = ifelse(Type == "Panel", "Sensory", Type)) %>%
    #   unique()
    # 
    # if (min(var_imp_plot_s_data$alpha) == 1){
    #   
    # temp_p <- var_imp_plot_s_data %>%
    #   ggplot(aes(
    #     x = var,
    #     y = value,
    #     fill = Type
    #   ))
    #   
    # } else {
    #   
    #   temp_p <- var_imp_plot_s_data %>%
    #     ggplot(aes(
    #       x = var,
    #       y = value,
    #       fill = Type,
    #       alpha = alpha
    #     ))
    #   
    #   
    # }
    # 
    # var_imp_plot_s <- temp_p +
    #   geom_bar(stat = "identity", position = "dodge") +
    #   coord_flip() +
    #   scale_x_discrete(name = "") +
    #   scale_y_continuous(name = "",
    #                      labels = scales::label_percent(accuracy = 1)) +
    #   scale_fill_manual(
    #     values = c(gsk_purple, gsk_teal),
    #     breaks = c("Sensory", "Respondent"),
    #     labels = c("Importance", "Importance"),
    #     name = ""
    #   ) +
    #   geom_text(
    #     aes(
    #       x = var,
    #       y = value,
    #       hjust = 1,
    #       vjust = 0,
    #       label = text
    #     ),
    #     #fill = "white",
    #     color = text_col,
    #     size = 0.2 * font_size,
    #     family = font_name,
    #     #label.r = unit(0, "lines")
    #   ) +
    #   theme(
    #     axis.line = element_blank(),
    #     panel.background = element_blank(),
    #     plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
    #     panel.grid = element_blank(),
    #     legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
    #     legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
    #     legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
    #     axis.ticks = element_blank(),
    #     axis.text = element_blank(),
    #     axis.ticks.x = element_blank(),
    #     legend.text = element_text(
    #       size = font_size,
    #       family = font_name,
    #       color = text_col
    #     ),
    #     axis.title = element_blank(),
    #     legend.title = element_blank(),
    #     panel.border = element_blank(),
    #     legend.position = "bottom"
    #   ) +
    #   scale_y_reverse() +
    #   guides(alpha = FALSE)
    
    #var_imp_plot_s
    
    local_opt_chart <-  prof_comp_plot_full +
      # plot_layout(ncol = 2, widths = c(4, 5)) +
      plot_annotation(
        title = str_c(
          "Predicted liking improvement of ",
          formatC(
            improve_val,
            format = "f",
            width = 5,
            digits = 3
          ),
          " if recommended adjustments made."
        )
      )
    
    }
    
    return(local_opt_chart)
    
  }


# load results
output_file_path_data <-
  file.path("output",
            "modeling_results",
            source_stem,
            "lo_res.rds")

local_opt_chart_list_list <- local_opt_list_list %>%
  map(~map(., make_local_opt_chart))

# save results
chart_file_path <-
  file.path(
    "output",
    "modeling_results",
    source_stem,
    str_c(source_stem, "_local_opt_chart_list_list.RDS")
  )

saveRDS(local_opt_chart_list_list, file = chart_file_path)

