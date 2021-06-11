
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

# load data and compute panel means

clean_data_path <-
  file.path("input",
            "cleaned_data",
            str_c(source_stem, "_cleaned_data.Rdata"))

load(file = clean_data_path)

range_data <- cleaned_data$x %>%
  group_by(Stim) %>%
  summarize_all(mean, na.rm = TRUE) %>%
  ungroup() %>%
  select(Stim, starts_with("X")) %>%
  pivot_longer(starts_with("X"), names_to = "var", values_to = "value") %>%
  group_by(var) %>%
  summarize(Min = min(value), Max = max(value)) %>%
  mutate(Range = Max - Min) %>%
  select(var, Range) %>%
  left_join(cleaned_keys$x) %>%
  mutate(Variable = tools::toTitleCase(var_name)) %>%
  select(Variable, Range)

# prepare sample means for heatmap table ----

liking_means <- cleaned_data$y %>%
  group_by(Stim) %>%
  summarize(Y = mean(Y))

full_key <- cleaned_keys$x %>%
  bind_rows(tibble(var = "Y", var_name = "liking"))

full_info <- var_imp_info %>%
  rename(Variable = var, Importance = value) %>%
  select(Variable, Importance)

levels(full_info$Variable) <-
  c(levels(full_info$Variable), "Liking")

full_info <- full_info %>%
  bind_rows(tibble(Variable = "Liking", Importance = 1))

full_info <- full_info %>%
  arrange(desc(Importance))

heatmap_table_info <- cleaned_data$x %>%
  select(Stim, starts_with("X")) %>%
  group_by(Stim) %>%
  summarize_all(mean, na.rm = TRUE) %>%
  ungroup() %>%
  left_join(cleaned_keys$s) %>%
  left_join(liking_means) %>%
  select(-Stim) %>%
  pivot_longer(-Stim_Name, names_to = "var", values_to = "Average") %>%
  mutate(Stimulus = as.factor(tools::toTitleCase(as.character(Stim_Name)))) %>%
  left_join(full_key %>%
              mutate(Variable = tools::toTitleCase(var_name))) %>%
  select(Stimulus, Variable, Average) %>%
  inner_join(full_info) %>%
  mutate(Variable = fct_reorder(factor(Variable), Importance)) %>%
  group_by(Variable) %>%
  mutate(fill = rescale(Average)) %>%
  mutate(ave_label = formatC(
    Average,
    format = "f",
    width = 5,
    digits = 2
  )) %>%
  ungroup() %>%
  mutate(Variable = fct_rev(Variable))

prods_in_order <- heatmap_table_info %>%
  filter(Variable == "Liking") %>%
  arrange(desc(Average)) %>%
  pull(Stimulus) %>%
  as.character()

# define labels

heatmap_table_labels <- heatmap_table_info %>%
  filter(Variable == "Liking")

# make heatmap table ----

imp_vals <- heatmap_table_info %>% 
  distinct(Variable, Importance) %>% 
  pull(Importance)

imp_thresh <- median(imp_vals)

heatmap_table <- heatmap_table_info %>%
  filter(Importance >= imp_thresh) %>% 
  mutate(Variable = fct_rev(Variable)) %>%
  mutate(Stimulus = factor(Stimulus, levels = prods_in_order)) %>%
  ggplot(aes(x = Stimulus, y = Variable, fill = fill)) +
  geom_tile() +
  scale_fill_gradient(
    low = gsk_grad_med,
    high = gsk_grad_low,
    name = "Relative Means",
    breaks = c(0, 0.5, 1),
    labels = c("Low", "Medium", "High"),
    trans = 'reverse'
  ) +
  geom_label(
    aes(
      hjust = 0.5,
      vjust = 0.5,
      label = ave_label
    ),
    fill = "white",
    color = text_col,
    size = 0.3 * font_size,
    family = font_name,
    label.r = unit(0, "lines"),
    data = heatmap_table_labels
  ) +
  scale_x_discrete(position = "top", guide = guide_axis(n.dodge = 3)) +
  theme(
    axis.line = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
    legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
    legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
    text = element_text(
      size = font_size,
      family = font_name,
      color = text_col,
    ),
    legend.position = "bottom",
    axis.title = element_blank()
  )

heatmap_table

# make variable importance plots ----

# overall plot

var_imp_plot <- var_imp_info %>%
  mutate(Type = ifelse(Type == "Panel", "Sensory", Type)) %>%
  group_by(Type) %>% 
  summarize(value = sum(value)) %>% 
  mutate(text = format_percent(value)) %>% 
  ggplot(aes(x = Type, y = value, fill = Type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "",
                     labels = scales::label_percent(accuracy = 1)) +
  scale_fill_manual(values = c(gsk_purple, gsk_teal) %>% set_names(c("Sensory", "Respondent")),
                    name = "") +
  geom_label(
    aes(
      x = Type,
      y = 0,
      hjust = 0,
      vjust = 0.5,
      label = text
    ),
    fill = "white",
    color = text_col,
    size = 0.3 * font_size,
    family = font_name,
    label.r = unit(0, "lines")
  ) +
  theme(
    axis.line = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
    legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
    legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
    axis.text = element_text(
      size = font_size,
      family = font_name,
      color = text_col,
    ),
    legend.text = element_text(
      size = font_size,
      family = font_name,
      color = text_col
    ),
    legend.title = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom"
  )

var_imp_plot

# sensory importance only

imp_thresh <- 0.02

var_imp_plot_s <- var_imp_info %>%
  filter(Type == "Panel") %>%
  mutate(value = value / sum(value), text = format_percent(value)) %>% 
  filter(value >= imp_thresh) %>% 
  mutate(Type = ifelse(Type == "Panel", "Sensory", Type)) %>%
  ggplot(aes(x = var, y = value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "",
                     labels = scales::label_percent(accuracy = 1)) +
  scale_fill_manual(values = c(gsk_purple, gsk_teal) %>% set_names(c("Sensory", "Respondent")),
                    name = "") +
  geom_label(
    aes(
      x = var,
      y = 0,
      hjust = 0,
      vjust = 0.5,
      label = text
    ),
    fill = "white",
    color = text_col,
    size = 0.3 * font_size,
    family = font_name,
    label.r = unit(0, "lines")
  ) +
  theme(
    axis.line = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
    legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
    legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
    axis.text = element_text(
      size = font_size,
      family = font_name,
      color = text_col,
    ),
    legend.text = element_text(
      size = font_size,
      family = font_name,
      color = text_col
    ),
    legend.title = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
  )

var_imp_plot_s

# cluster importance plots ----

load(file = file.path("output", "data_exploration", source_stem, "clust_info_list.rdata"))

clust_info <- clust_info_list$clust_info

# wordcloud

imp_thresh <- 0.01

clust_imp_info <- var_imp_info %>%
  filter(Type == "Panel") %>% 
  mutate(value = value / sum(value), text = format_percent(value)) %>% 
  full_join(clust_info %>% mutate(var = toTitleCase(var))) %>% 
  mutate(full_var = str_c(text, ": ", var)) %>%
  select(var, full_var, Cluster, value) %>% 
  arrange(desc(value))
  
clust_imp_plot_data <- clust_imp_info %>% 
  group_by(Cluster) %>% 
  summarize(value = sum(value)) %>% 
  ungroup() %>% 
  arrange(desc(value)) %>%
  mutate(new_clust = 1:nrow(.)) %>% 
  mutate(text = format_percent(value)) %>%
  mutate(Label = str_c("Cluster ", new_clust, ": ", text)) %>% 
  mutate(Label = fct_reorder(factor(Label), -value)) %>% 
  select(Cluster, Label)

set.seed(123)
cluster_wc_plot <- clust_imp_plot_data %>% 
  left_join(clust_imp_info) %>% 
  filter(value >= imp_thresh) %>% 
  #select(Label, full_var, value) %>%
  count(Label, full_var) %>%
  #ggplot(aes(label = full_var, size = value)) +
  ggplot(aes(label = full_var, size = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 3) + 
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    text = element_text(
      size = font_size,
      family = font_name
    ),
    strip.text = element_text(
      face = "bold",
      size = font_size,
      family = font_name,
      vjust = 0.5
    )
  ) +
  facet_wrap( ~ Label)

cluster_wc_plot

# phylogentic tree

name_recode_vect <- clust_imp_info %>% 
  select(var, full_var) %>% 
  deframe()

clust_data <- clust_info_list$clust_data
names(clust_data) <- name_recode_vect[toTitleCase(names(clust_data))]

clust_phylo_plot <- clust_data %>%
  make_cluster_list(num_clusts_k = clust_info_list$num_clusts) %>% 
  .$plot

clust_out_dir <- file.path("output",
                           "data_exploration",
                           source_stem)

clust_phylo_plot <- readRDS(file.path(clust_out_dir, "phylo_plot.rds"))


# respondent variables ----

var_imp_info %>% 
  filter(Type == "Respondent") %>% 
  pull(var)

imp_thresh <- 0.03

var_imp_plot_r <- var_imp_info %>% 
  filter(Type == "Respondent") %>%
  mutate(join_var = str_to_title(var)) %>% 
  left_join(cleaned_keys$oh %>% mutate(join_var = str_to_title(new_var_name))) %>% 
  group_by(var_name, Type) %>% 
  summarize(sum_value = sum(value)) %>% 
  ungroup() %>% 
  mutate(value = sum_value / sum(sum_value), text = format_percent(value)) %>% 
  filter(value >= imp_thresh) %>% 
  ggplot(aes(x = fct_reorder(var_name, value), y = value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "",
                     labels = scales::label_percent(accuracy = 1)) +
  scale_fill_manual(values = c(gsk_purple, gsk_teal) %>% set_names(c("Sensory", "Respondent")),
                    name = "") +
  geom_label(
    aes(
      x = var_name,
      y = 0,
      hjust = 0,
      vjust = 0.5,
      label = text
    ),
    fill = "white",
    color = text_col,
    size = 0.3 * font_size,
    family = font_name,
    label.r = unit(0, "lines")
  ) +
  theme(
    axis.line = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
    legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
    legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
    axis.text = element_text(
      size = font_size,
      family = font_name,
      color = text_col,
    ),
    legend.text = element_text(
      size = font_size,
      family = font_name,
      color = text_col
    ),
    legend.title = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
  )

var_imp_plot_r

load(file = file.path("output", "data_exploration", source_stem, "demo_clust_info_list.rdata"))

clust_info <- demo_clust_info_list$clust_info

# wordcloud

imp_thresh <- 0.015

clust_imp_info <- var_imp_info %>%
  filter(Type == "Respondent") %>%
  mutate(value = value / sum(value), text = format_percent(value)) %>% 
  full_join(clust_info %>% mutate(var = toTitleCase(var))) %>% 
  mutate(full_var = str_c(text, ": ", var)) %>%
  select(var, full_var, Cluster, value) %>% 
  arrange(desc(value))

clust_imp_plot_data <- clust_imp_info %>% 
  group_by(Cluster) %>% 
  summarize(value = sum(value)) %>% 
  ungroup() %>% 
  arrange(desc(value)) %>%
  mutate(new_clust = 1:nrow(.)) %>% 
  mutate(text = format_percent(value)) %>%
  mutate(Label = str_c("Cluster ", new_clust, ": ", text)) %>% 
  mutate(Label = fct_reorder(factor(Label), -value)) %>% 
  select(Cluster, Label)

set.seed(123)
demo_cluster_wc_plot <- clust_imp_plot_data %>% 
  left_join(clust_imp_info) %>% 
  filter(value >= imp_thresh) %>% 
  #select(Label, full_var, value) %>%
  count(Label, full_var) %>%
  #ggplot(aes(label = full_var, size = value)) +
  ggplot(aes(label = full_var, size = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 3) + 
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    text = element_text(
      size = font_size,
      family = font_name
    ),
    strip.text = element_text(
      face = "bold",
      size = font_size,
      family = font_name,
      vjust = 0.5
    )
  ) +
  facet_wrap( ~ Label)

demo_cluster_wc_plot

# phylogentic tree

name_recode_vect <- clust_imp_info %>% 
  select(var, full_var) %>% 
  deframe()

clust_data <- demo_clust_info_list$clust_data
names(clust_data) <- name_recode_vect[toTitleCase(names(clust_data))]

# TODO: Fix NAs in clust_data names

# demo_clust_phylo_plot <- clust_data %>%
#   make_cluster_list(num_clusts_k = clust_info_list$num_clusts) %>% 
#   .$plot

# plot local optimizations ----

chart_file_path <-
  file.path(
    "output",
    "modeling_results",
    source_stem,
    str_c(source_stem, "_local_opt_chart_list_list.RDS")
  )

local_opt_chart_list_list <- readRDS(file = chart_file_path)

# add model fits ----------------------------------------------------------

# model fitting charts
info_file_path <-
  file.path(
    "output",
    "modeling_results",
    source_stem,
    str_c(source_stem, "_selected_model_info.RData")
  )
load(file = info_file_path)

means_comp_plot_test <-
  selected_model_info$means_comp_plot_test %>%
  add_custom_theme(body_bg_col = body_bg_col,
                   text_col = text_col,
                   axis_labels = TRUE) +
  xlab("\nLiking") +
  ylab("Predicted Liking\n")

means_comp_plot_full <-
  selected_model_info$means_comp_plot_full %>%
  add_custom_theme(body_bg_col = body_bg_col,
                   text_col = text_col,
                   axis_labels = TRUE) +
  xlab("\nLiking") +
  ylab("Predicted Liking\n")

# add cluster analysis plots ----------------------------------------------------

#cluster_pal <- c(gsk_blue, gsk_purple, gsk_green2)

# data quality charts
dq_file_path <-
  file.path("output",
            "data_exploration",
            source_stem,
            "cluster_analysis_plots.Rdata")
load(file = dq_file_path)

cluster_liking_plot <- cluster_liking_plot %>%
  add_custom_theme(body_bg_col = body_bg_col,
                   text_col = text_col,
                   axis_labels = TRUE) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_blank(),
    axis.title.x = element_blank()
  ) + 
  scale_fill_manual(values = cluster_pal)

cluster_liking_plot

cluster_ci_plot <- cluster_ci_plot %>%
  add_custom_theme(body_bg_col = body_bg_col,
                   text_col = text_col,
                   axis_labels = TRUE)  +
  theme(
    legend.position = "bottom",
    axis.text.y = element_blank(),
    axis.title = element_blank()
  ) + 
  scale_color_manual(values = cluster_pal)

cluster_ci_plot

#  make cluster barplots -----------------------------------------------------

output_dir <- file.path("output", "modeling_results", source_stem)

# info_file_path <-
#   file.path(output_dir, str_c(source_stem, "_cluster_vip.RData"))
# 
# load(file = info_file_path)
# 
# cluster_vip_info <- vip_info[["full"]]
# 
# imp_thresh <- 0.025
# 
# cluster_vip_plot <- cluster_vip_info %>%
#   mutate(join_var = str_to_title(var_name)) %>% 
#   rename(var = var_name) %>% 
#   left_join(cleaned_keys$oh %>% 
#               mutate(join_var = str_to_title(new_var_name)), by = "join_var") %>% 
#   group_by(var_name) %>% 
#   summarize(vip = sum(vip)) %>% 
#   ungroup() %>% 
#   filter(vip >= imp_thresh) %>% 
#   mutate(text = format_percent(vip), var_name = fct_reorder(factor(var_name), vip)) %>%
#   rename(var = var_name, value = vip) %>%
#   ggplot(aes(x = fct_reorder(var, value), y = value)) +
#   geom_bar(stat = "identity", fill = gsk_blue) +
#   coord_flip() +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "",
#                      labels = scales::label_percent(accuracy = 1)) +
#   geom_label(
#     aes(
#       x = var,
#       y = 0,
#       hjust = 0,
#       vjust = 0.5,
#       label = text
#     ),
#     fill = "white",
#     color = text_col,
#     size = 0.3 * font_size,
#     family = font_name,
#     label.r = unit(0, "lines")
#   ) +
#   theme(
#     axis.line = element_blank(),
#     panel.background = element_blank(),
#     plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
#     panel.grid = element_blank(),
#     legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
#     legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
#     legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
#     axis.text = element_text(
#       size = font_size,
#       family = font_name,
#       color = text_col,
#     ),
#     legend.text = element_text(
#       size = font_size,
#       family = font_name,
#       color = text_col
#     ),
#     legend.title = element_blank(),
#     panel.border = element_blank(),
#     legend.position = "none"
#   )
# 
# cluster_vip_plot

# make box and bar plots to explain clustering ------

demo_data <- cleaned_data$d %>%
  select(-ID_Y)

demo_key <- cleaned_keys$d

vip_thresh <- 0.03

all_data <- demo_data %>% 
  pivot_longer(starts_with("D"), names_to = "var", values_to = "value") %>% 
  group_by(var) %>% 
  summarize(n = n(), prop = mean(value)) %>% 
  ungroup() %>% 
  mutate(Cluster = "All") %>% 
  relocate(Cluster)
# 
# lotus_data <- demo_data %>% 
#   pivot_longer(starts_with("D"), names_to = "var", values_to = "value") %>% 
#   group_by(Cluster, var) %>% 
#   summarize(n = n(), prop = mean(value)) %>% 
#   ungroup() %>% 
#   mutate(Cluster = str_c(Cluster, " (n = ", n, ")")) %>% 
#   bind_rows(all_data) %>% 
#   left_join(demo_key) %>% 
#   left_join(cluster_vip_info) %>% 
#   filter(vip >= vip_thresh) %>% 
#   select(-var) %>% 
#   mutate(text = format_percent(vip)) %>% 
#   mutate(full_var = str_c(text, ": ", var_name)) %>%
#   mutate(full_var = fct_reorder(full_var, vip)) %>% 
#   arrange(desc(full_var)) %>% 
#   select(Sample = Cluster, Attribute = full_var, Value = prop)
# 
# cluster_lotus_plot <- lotus_data %>% 
#   make_lotus_plot(col_pal = cluster_pal, min_val = 0, max_val = 1.1*max(lotus_data$Value))
#  
# reconstruct demographic data using one hot encoding lookup table

shift_thresh <- 0.01

header_vars <- c("ID_Y", "Cluster")

cleaned_keys$d %>% 
  left_join(cleaned_keys$oh, by = c("var_name" = "new_var_name"))

# demo_data_fact <- cleaned_data$d %>%
#   mutate(Cluster = fct_rev(factor(str_c("Cluster ", as.character(Cluster))))) %>%
#   pivot_longer(-one_of(header_vars), names_to = "var", values_to = "level") %>%
#   count(Cluster, var, level) %>%
#   group_by(Cluster, var) %>%
#   mutate(total = sum(n), prop = n / total) %>%
#   ungroup() %>%
#   left_join(demo_key) %>%
#   select(var_name, Cluster, level, prop) %>%
#   left_join(cluster_vip_info) %>%
#   mutate(vip = if_else(is.na(vip), 0, vip)) %>%
#   mutate(var_name = fct_reorder(factor(var_name), vip)) %>%
#   arrange(desc(vip), var_name, Cluster) %>%
#   mutate(level = factor(level, levels = c(0, 1), labels = c("No", "Yes"))) %>%
#   group_by(Cluster, var_name) %>%
#   mutate(
#     prop_shift = ifelse(prop > shift_thresh, shift_thresh, prop),
#     loc = cumsum(prop) - prop_shift
#   ) %>%
#   ungroup() %>%
#   mutate(level = fct_reorder(level, loc)) %>%
#   arrange(desc(var_name), Cluster, level) %>%
#   mutate(label = str_c(level, ": ", format_percent(prop)))

# assign fill colors

# demo_fill_colors <- demo_data_fact %>%
#   select(var_name, level) %>%
#   unique() %>%
#   arrange(var_name, level) %>%
#   group_by(var_name) %>%
#   mutate(rank = rank(level)) %>%
#   mutate(color = ifelse(rank == 1, gsk_blue, ifelse(rank == 2, gsk_teal, gsk_yellow))) %>%
#   ungroup() %>%
#   select(level, color) %>%
#   unique() %>%
#   deframe()

# poss_var_names <- demo_data_fact$var_name %>%
#   levels() %>%
#   rev()

# cluster_barplot_list <- vector("list")
# 
# num_vars <- poss_var_names %>%
#   length()
# vars_per_slide <- 3
# 
# num_demo_fact_slides <- num_vars / vars_per_slide %>%
#   ceiling()
# 
# i <- 1
# for (i in 1:num_demo_fact_slides) {
#   select_vars <- poss_var_names[vars_per_slide * (i - 1) + 1:vars_per_slide]
# 
#   cluster_barplot_list[[i]] <- demo_data_fact %>%
#     filter(var_name %in% select_vars) %>%
#     ggplot(aes(
#       x = prop,
#       y = Cluster,
#       fill = fct_rev(level)
#     )) +
#     geom_bar(stat = "identity") +
#     facet_wrap( ~ fct_rev(var_name), ncol = 1) +
#     scale_x_continuous(labels = scales::percent_format()) +
#     theme(
#       axis.line = element_blank(),
#       panel.background = element_blank(),
#       plot.background = element_rect(fill = body_bg_col, color = body_bg_col),
#       panel.grid = element_blank(),
#       legend.background = element_rect(fill = body_bg_col, color = body_bg_col),
#       legend.key = element_rect(fill = body_bg_col, color = body_bg_col),
#       legend.box.background = element_rect(fill = body_bg_col, color = body_bg_col),
#       text = element_text(
#         size = font_size,
#         family = font_name,
#         color = text_col,
#       ),
#       legend.position = "none",
#       axis.title = element_blank()
#     ) +
#     geom_label(
#       aes(x = loc, y = Cluster, label = label),
#       hjust = 1,
#       vjust = 0.5,
#       fill = "white",
#       color = text_col,
#       size = 0.3 * font_size,
#       family = font_name,
#       label.r = unit(0, "lines")
#     ) +
#     scale_fill_manual(values = demo_fill_colors)
# 
# 
# }
# 
# cluster_barplot_list[[2]]

# Create slides

pptx_obj <-
  file.path("input", "templates", template_filename) %>%
  read_pptx()

# Create new slide and add title
title_value <- "DOL Visualizations"
pptx_obj <- pptx_obj %>%
  add_slide(layout = header_layout, master = master_value) %>%
  ph_with(value = title_value, location = ph_location_type(type = header_type))

# goodness of fits

section_title <- "Goodness of Fit"

pptx_obj <- pptx_obj %>%
  add_slide(layout = section_layout, master = master_value) %>%
  ph_with(value = section_title,
          location = ph_location_label(ph_label = section_ph)) %>%
  add_slide_with_chart(means_comp_plot_test, "Goodness of Fit")

# add variable importance percentages

section_title <- "Variable Importance Percentages"

pptx_obj <- pptx_obj %>%
  add_slide(layout = section_layout, master = master_value) %>%
  ph_with(value = section_title,
          location = ph_location_label(ph_label = section_ph)) %>%
  add_slide_with_chart(var_imp_plot, "Breakdown by Type") %>% 
  add_slide_with_chart(var_imp_plot_s, "Top Sensory Variables") %>% 
  add_slide_with_chart(cluster_wc_plot, "All Sensory Variables by Cluster") %>%
  add_slide_with_chart(clust_phylo_plot, "All Sensory Variables by Cluster") %>% 
  add_slide_with_chart(var_imp_plot_r, "Top Respondent Variables") %>% 
  add_slide_with_chart(demo_cluster_wc_plot, "All Respondent Variables by Cluster") 

# %>%
#   add_slide_with_chart(demo_clust_phylo_plot, "All Respondent Variables by Cluster")
  
# add heatmap table

section_title <- "Relative Sensory Means"
chart_to_plot <- heatmap_table

pptx_obj <- pptx_obj %>%
  add_slide(layout = section_layout, master = master_value) %>%
  ph_with(value = section_title,
          location = ph_location_label(ph_label = section_ph)) %>%
  add_slide_with_chart(chart_to_plot, section_title)

# add local optimizations

section_names <- local_opt_chart_list_list %>%
  names() %>%
  .[1]

section_name <- section_names[[1]]

for (section_name in section_names) {
  section_title <- str_c("Local Optimization: ", section_name)
  section_sub <- "Product by Product Visualizations"
  
  pptx_obj <- pptx_obj %>%
    add_slide(layout = section_layout, master = master_value) %>%
    ph_with(value = section_title,
            location = ph_location_label(ph_label = section_ph)) %>%
    ph_with(value = section_sub,
            location = ph_location_label(ph_label = section_ph_2))
  
  chart_to_plot_list <- local_opt_chart_list_list[[section_name]]

  prod_names <- chart_to_plot_list %>%
    names()
  
  prod_name <- prod_names[[1]]
  
  for (prod_name in prod_names) {
    chart_to_plot <- chart_to_plot_list[[prod_name]]
    
    slide_title <- str_c(section_title, ", Product: ", prod_name)
    
    if (is.null(chart_to_plot)) {
      slide_bullets <- tribble(
        ~ str,
        ~ lvl,
        str_c(
          prod_name,
          " is locally optimal for each individual model attribute for this consumer group."
        ),
        1
      ) %>%
        make_ul()
      
      pptx_obj <- pptx_obj %>%
        add_slide(layout = title_content_layout, master = master_value) %>%
        ph_with(value = slide_title,
                location = ph_location_type(type = title_type)) %>%
        ph_with(value = slide_bullets,
                location = ph_location_label(ph_label = "Content Placeholder 15"))
      
    } else {
      pptx_obj <- pptx_obj %>%
        add_slide_with_chart(chart_to_plot, slide_title)
      
    }
    
  }
  
}

# add global optimization plots

chart_file_path <- file.path("output", "modeling_results", source_stem, str_c(source_stem, "_global_opt_charts.RDS"))
global_opt_charts <- readRDS(file = chart_file_path)

global_opt_chart_list_list <- global_opt_charts$global_opt_chart_list_list
global_cluster_opt_chart <- global_opt_charts$global_cluster_opt_chart

# plot product by product visualizations

section_title <- "Global Optimization"
section_sub <- "Product by Product Visualizations"

global_opt_chart_list <- global_opt_chart_list_list[["All"]]

pptx_obj <- pptx_obj %>%
  add_slide(layout = section_layout, master = master_value) %>%
  ph_with(value = section_title,
          location = ph_location_label(ph_label = section_ph)) %>%
  ph_with(value = section_sub,
          location = ph_location_label(ph_label = section_ph_2))

prod_names <- global_opt_chart_list %>%
  names()

prod_name <- prod_names[[1]]

for (prod_name in prod_names) {
  chart_to_plot <- global_opt_chart_list[[prod_name]]
  
  slide_title <- str_c(section_title, ", Product: ", prod_name)
  
  pptx_obj <- pptx_obj %>%
    add_slide_with_chart(chart_to_plot, slide_title)
  
  
}

# add cluster plots

section_title <- "Cluster Analysis Overview"

pptx_obj <- pptx_obj %>%
  add_slide(layout = section_layout, master = master_value) %>%
  ph_with(value = section_title,
          location = ph_location_label(ph_label = section_ph)) %>%
  add_slide_with_chart(cluster_liking_plot, "Liking Frequencies by Cluster") %>%
  add_slide_with_chart(cluster_ci_plot, "Liking Means by Cluster")

# section_title <- "Cluster Analysis Characterization"
# 
# pptx_obj <- pptx_obj %>%
#   add_slide(layout = section_layout, master = master_value) %>%
#   ph_with(value = section_title,
#           location = ph_location_label(ph_label = section_ph)) %>%
#   add_slide_with_chart(cluster_vip_plot, "Variable Importance") 

# %>% 
#   add_slide_with_chart(cluster_lotus_plot, "Cluster Characterization")
# 
# for (i in 1:num_demo_fact_slides){
# 
#   pptx_obj <- pptx_obj %>%
#     add_slide_with_chart(cluster_barplot_list[[i]], "Barplots of Categorical Predictors")
# 
# }

# add product by product DOL visualizations

section_names <- full_dol_list %>%
  names() %>%
  .[2:3]

section_name <- section_names[[1]]

for (section_name in section_names) {
  section_title <- str_c("Local Optimization: ", section_name)
  section_sub <- "Product by Product Local Optimizations"

  pptx_obj <- pptx_obj %>%
    add_slide(layout = section_layout, master = master_value) %>%
    ph_with(value = section_title,
            location = ph_location_label(ph_label = section_ph)) %>%
    ph_with(value = section_sub,
            location = ph_location_label(ph_label = section_ph_2))

  chart_to_plot_list <- local_opt_chart_list_list[[section_name]]

  prod_names <- chart_to_plot_list %>%
    names()

  prod_name <- prod_names[[1]]

  for (prod_name in prod_names) {
    chart_to_plot <- chart_to_plot_list[[prod_name]]

    slide_title <- str_c(section_title, ", Product: ", prod_name)

    if (is.null(chart_to_plot)) {
      slide_bullets <- tribble(
        ~ str,
        ~ lvl,
        str_c(
          prod_name,
          " is locally optimal for each individual model attribute for this consumer group."
        ),
        1
      ) %>%
        make_ul()

      pptx_obj <- pptx_obj %>%
        add_slide(layout = title_content_layout, master = master_value) %>%
        ph_with(value = slide_title,
                location = ph_location_type(type = title_type)) %>%
        ph_with(value = slide_bullets,
                location = ph_location_label(ph_label = "Content Placeholder 15"))

    } else {
      pptx_obj <- pptx_obj %>%
        add_slide_with_chart(chart_to_plot, slide_title)

    }

  }

}

# plot global optima comparisons

section_title <- "Global Optimization"
section_sub <- "Comparison of Optima"

pptx_obj <- pptx_obj %>%
  add_slide(layout = section_layout, master = master_value) %>%
  ph_with(value = section_title,
          location = ph_location_label(ph_label = section_ph)) %>%
  ph_with(value = section_sub,
          location = ph_location_label(ph_label = section_ph_2))

slide_title <- "Comparison of Optima"
chart_to_plot <- global_cluster_opt_chart

pptx_obj <- pptx_obj %>%
  add_slide_with_chart(chart_to_plot, slide_title)

# plot global optima product by product visualizations by cluster

section_names <- global_opt_chart_list_list %>%
  names() %>%
  .[2:3]

section_name <- section_names[[1]]

for (section_name in section_names) {
  section_title <- str_c("Global Optimization: ", section_name)
  section_sub <- "Product by Product Visualizations"

  pptx_obj <- pptx_obj %>%
    add_slide(layout = section_layout, master = master_value) %>%
    ph_with(value = section_title,
            location = ph_location_label(ph_label = section_ph)) %>%
    ph_with(value = section_sub,
            location = ph_location_label(ph_label = section_ph_2))

  global_opt_chart_list <- global_opt_chart_list_list[[section_name]]

  prod_names <- global_opt_chart_list %>%
    names()

  prod_name <- prod_names[[1]]

  for (prod_name in prod_names) {
    chart_to_plot <- global_opt_chart_list[[prod_name]]

    slide_title <- str_c(section_title, ", Product: ", prod_name)

    pptx_obj <- pptx_obj %>%
      add_slide_with_chart(chart_to_plot, slide_title)

  }

}

# Write slides out to PowerPoint

output_file <-
  str_c(Sys.Date(), " - ", source_label, " - Machine Learning Automated Output.pptx")

output_dir <- file.path("output", "reporting")

if (!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}

pptx_obj %>%
  print(target = file.path(output_dir, output_file))
