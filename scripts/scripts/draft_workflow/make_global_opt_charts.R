
# load data and compute panel means

clean_data_path <-
  file.path("input",
            "cleaned_data",
            str_c(source_stem, "_cleaned_data.Rdata"))

load(file = clean_data_path)

# calculate panel means

panel_data <- cleaned_data$x %>%
  select(Stim, starts_with("X")) %>%
  group_by(Stim) %>%
  summarize_all(mean, na.rm = TRUE) %>%
  ungroup() %>%
  left_join(cleaned_keys$s) %>%
  select(Stim_Name, starts_with("X"))

# calculate range data

range_data <- cleaned_data$x %>%
  group_by(Stim) %>%
  summarize_all(mean, na.rm = TRUE) %>%
  ungroup() %>%
  select(Stim, starts_with("X")) %>%
  pivot_longer(starts_with("X"), names_to = "var", values_to = "value") %>%
  group_by(var) %>%
  summarize(Min = min(value), Max = max(value)) %>%
  mutate(Range = Max - Min) %>%
  select(var, Max, Range) %>%
  left_join(cleaned_keys$x) %>%
  mutate(Variable = tools::toTitleCase(var_name)) %>%
  select(Variable, Max, Range)

# load variable importance info ----

visualization_info_path <-
  file.path(
    "output",
    "modeling_results",
    source_stem,
    str_c(source_stem, "_visualization_info.RData")
  )

load(file = visualization_info_path)

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

make_global_opt_chart_list <-
  function(global_opt, prop_thresh = 0.1, alpha_val = 0.5, opt_min = 0, opt_max = 100) {
    
#    browser()
    
    opt_data <- global_opt %>%
      pivot_longer(starts_with("X"),
                   names_to = "var",
                   values_to = "Optimum") %>%
      left_join(cleaned_keys$x) %>%
      mutate(Variable = tools::toTitleCase(var_name)) %>%
      select(Variable, Optimum) %>%
      inner_join(rel_var_imp_info) %>% 
      mutate(Max = pmax(Optimum, Max))
    
    opt_plot_data_list <- panel_data %>%
      pivot_longer(starts_with("X"),
                   names_to = "var",
                   values_to = "Value") %>%
      left_join(cleaned_keys$x) %>%
      mutate(Variable = tools::toTitleCase(var_name)) %>%
      select(Stim_Name, Variable, Value) %>%
      inner_join(opt_data) %>%
      split(.$Stim_Name)
    
    flag_thresh <- 0.05
    
    opt_plot_data <- opt_plot_data_list[[1]]
    
    make_global_opt_chart <-
      function(opt_plot_data, prop_thresh = prop_thresh, alpha_val = alpha_val) {
        
        #browser()
        
        o_data <- opt_plot_data %>%
          mutate(alpha = if_else(abs(Value - Optimum) <= prop_thresh * Range, 1, alpha_val)) %>%
          rename(Sample = Value) %>%
          select(-Stim_Name) %>%
          mutate(Variable = fct_reorder(factor(Variable), Importance)) %>%
          mutate(Diff = abs(Sample - Optimum),
                 flag = (Diff / Range <= flag_thresh)) %>%
          pivot_longer(Sample:Optimum,
                       names_to = "Source",
                       values_to = "Value") %>%
          mutate(Value = pmax(Value, opt_min)) %>%
          mutate(Value = pmin(Value, opt_max)) %>% 
          mutate(
            plot_value = Value / Max,
            plot_label = formatC(
              Value,
              format = "f",
              width = 5,
              digits = 1
            )
          ) %>%
          select(Variable, Source, flag, plot_value, plot_label, alpha)
        
        hjust_val <- 0
        
        #browser()
        prof_comp_plot_full <- o_data %>%
          ggplot(aes(
            x = Variable,
            y = plot_value,
            fill = Source,
            alpha = alpha
          )) +
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
            values = c(gsk_green2, gsk_red),
            breaks = c("Optimum", "Sample"),
            labels = c("Global Optimum", "Sample Profile"),
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

        var_imp_plot_s_data <- var_imp_info %>%
          filter(Type == "Panel") %>%
          mutate(value = value / sum(value), text = format_percent(value)) %>%
          filter(value >= imp_thresh) %>%
          inner_join(o_data %>%  distinct(Variable, alpha), by = c("var" = "Variable")) %>%
          mutate(Type = ifelse(Type == "Panel", "Sensory", Type)) %>%
          unique()
        
        var_imp_plot_s <- var_imp_plot_s_data %>%
          ggplot(aes(
            x = var,
            y = value,
            fill = Type, 
            alpha = alpha
          )) +
          geom_bar(stat = "identity", position = "dodge") +
          coord_flip() +
          scale_x_discrete(name = "") +
          scale_y_continuous(name = "",
                             labels = scales::label_percent(accuracy = 1)) +
          scale_fill_manual(
            values = c(gsk_purple, gsk_teal),
            breaks = c("Sensory", "Respondent"),
            labels = c("Importance", "Importance"),
            name = ""
          ) +
          geom_text(
            aes(
              x = var,
              y = value,
              hjust = 1,
              vjust = 0,
              label = text
            ),
            #fill = "white",
            color = text_col,
            size = 0.2 * font_size,
            family = font_name,
            #label.r = unit(0, "lines")
          ) +
          theme(
            axis.line = element_blank(),
            panel.background = element_blank(),
            plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
            panel.grid = element_blank(),
            legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
            legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
            legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.ticks.x = element_blank(),
            legend.text = element_text(
              size = font_size,
              family = font_name,
              color = text_col
            ),
            axis.title = element_blank(),
            legend.title = element_blank(),
            panel.border = element_blank(),
            legend.position = "bottom"
          ) +
          scale_y_reverse() +
          guides(alpha = FALSE)
        
        var_imp_plot_s
        
        global_opt_chart <- var_imp_plot_s + prof_comp_plot_full +
          plot_layout(ncol = 2, widths = c(4, 5)) +
          plot_annotation(
            title = "Attributes where sample is near optimal are highlighted."
          )
        
        
        return(global_opt_chart)
        
      }
    
    global_opt_chart_list <- opt_plot_data_list %>%
      map(make_global_opt_chart, prop_thresh = prop_thresh, alpha_val = alpha_val)
    
    return(global_opt_chart_list)
    
  }

# load results
output_file_path <-
  file.path("output",
            "modeling_results",
            source_stem,
            "global_opt_list.RDS")

global_opt_list <- readRDS(output_file_path)

global_opt <- global_opt_list[[1]]

global_opt_chart_list_list <- global_opt_list %>%
  map(make_global_opt_chart_list)

# make chart comparing optima

lotus_opt_data <- global_opt_list %>% 
  reduce(bind_rows) %>% 
  mutate(Stim = names(global_opt_list)) %>% 
  pivot_longer(-Stim, names_to = "var", values_to = "Value") %>% 
  left_join(cleaned_keys$x) %>%
  mutate(Variable = tools::toTitleCase(var_name)) %>% 
  inner_join(rel_var_imp_info) %>% 
  mutate(Variable = fct_reorder(factor(Variable), Importance)) %>% 
  select(Sample = Stim, Attribute = Variable, Value = Value)

global_cluster_opt_chart <- lotus_opt_data %>% 
  make_lotus_plot(col_pal = cluster_pal, min_val = 0, max_val = 1.1*max(lotus_opt_data$Value))

# save results
chart_file_path <-
  file.path(
    "output",
    "modeling_results",
    source_stem,
    str_c(source_stem, "_global_opt_charts.RDS")
  )

global_opt_charts <- list(global_opt_chart_list_list = global_opt_chart_list_list, global_cluster_opt_chart = global_cluster_opt_chart)

saveRDS(global_opt_charts, file = chart_file_path)
