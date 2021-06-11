# new_jar_list_file <- "input/raw_data/2021  STEP_JAR and sensory attributes reviewed Marie 25 March.xlsx"
# 
# new_jar_list <- new_jar_list_file %>%
#   readxl::excel_sheets() %>%
#   keep(grepl, pattern = "^JAR") %>%
#   set_names() %>%
#   map(readxl::read_excel, path = new_jar_list_file, skip = 1) %>%
#   enframe(name = "Code") %>%
#   unnest(value) %>%
#   pivot_longer(-Code, names_to = "attributes") %>%
#   mutate(across(everything(), snakecase::to_any_case)) %>%
#   filter(!is.na(value)) %>%
#   filter(attributes != "not_important") %>%
#   mutate(attributes = case_when(
#     attributes == "important" ~  "short_list",
#     attributes == "maybe_important" ~  "long_list"
#   )) %>%
#   mutate(value = gsub("_min", "min", value)) %>%
#   mutate(value = gsub("_at_1_", "_at1_", value)) %>%
#   mutate(value = sprintf(value, fmt = "sens_%s")) %>%
#   group_by(Code, attributes) %>%
#   nest() %>%
#   ungroup() %>%
#   mutate(sens_vars = map(data, pull, "value")) %>%
#   select(-data) %>%
#   pivot_wider(names_from = attributes, values_from = sens_vars) %>%
#   mutate(long_list = map2(short_list, long_list, c)) %>%
#   pivot_longer(-Code, names_to = "attributes", values_to = "sens_vars")
# 
# jar_list <- list(
#   long_list = "input/raw_data/2021 03 19 Aigora & GSK - STEP_JAR and sensory attributes.xlsx",
#   short_list = "input/raw_data/Aigora  GSK - STEP_JAR and sensory attributes ( MR reviewed).xlsx"
#   ) %>%
#   map(readxl::read_excel, sheet = "JAR and sensory attributes") %>%
#   enframe(name = "attributes") %>%
#   unnest(value) %>%
#   pivot_longer(-attributes, names_to = "Code") %>%
#   mutate(across(everything(), snakecase::to_any_case)) %>%
#   filter(!is.na(value)) %>%
#   mutate(value = gsub("_min", "min", value)) %>%
#   mutate(value = gsub("_at_1_", "_at1_", value)) %>%
#   mutate(value = sprintf(value, fmt = "sens_%s")) %>%
#   group_by(Code, attributes) %>%
#   nest() %>%
#   ungroup() %>%
#   mutate(sens_vars = map(data, pull, "value")) %>%
#   select(-data) %>%
#   anti_join(new_jar_list, by = "Code") %>%
#   bind_rows(new_jar_list)

imported_data_path <- file.path(
  "input",
  "imported_data",
  sprintf("tablet(36-66)_imported_data.rds", source_stem))

actionable_attributes_path <- file.path(
  "input",
  "raw_data",
  "GSK Centrum actionable attributes.xlsx"
)

sensory_list <- readRDS(imported_data_path) %>% 
  deframe()

actionable_attributes <- readxl::read_excel(actionable_attributes_path, sheet = "Tablet attribute list") %>%
  filter(!is.na(Actionable)) %>%
  pull(Variable) %>%
  janitor::make_clean_names()

sens_wide <- sensory_list$sensory %>%
  rename_with(janitor::make_clean_names) %>%
  select(
    product,
    any_of(actionable_attributes)
         ) %>% 
  group_by(product) %>% 
  summarize_all(mean, na.rm = TRUE) %>%
  rename_with(sprintf, fmt = "sens_%s", -product)

cons_data <- sensory_list$cons %>%
  select(
    product = Product_name,
    consid = ID,
    overall = Overall_opinion,
    t_jar_color = JAR_color,
    t_jar_aroma = JAR_aroma,
    t_num_swallow_ease = Swallow_ease
  ) %>%
  mutate(across(starts_with("t_jar_"),
                ~pmax(2, pmin(4, as.numeric(.))))) %>%
  mutate(
    across(c(starts_with("t_jar_")),
           factor, levels = 2:4, ordered = TRUE),
    across(c(starts_with("t_num_")),
           as.numeric),
    overall = as.numeric(overall)
  )


tg_plots <- cons_data %>%
  mutate(across(c(starts_with("t_")), as.character)) %>%
  pivot_longer(starts_with("t_")) %>%
  group_by(name, value) %>%
  summarise(overall = mean(overall),
            n = n()) %>%
  split(.$name) %>%
  imap(~{
    label <- gsub("t_[a-z]+_", "", .y)
    .x %>%
      ggplot(aes(value, overall)) +
      geom_point(, size = 5) +
      ggrepel::geom_text_repel(aes(label = n), min.segment.length = 1) +
      xlab(label) + ylab("Overall opinion") +
      ggtitle(label) +
      theme_bw()
  })

plot_file <- file.path(output_path, "plots", str_c(Sys.Date(), " - target.pdf"))
graphics.off()
pdf(plot_file, width = 8, height = 8)
tg_plots %>%
  walk(print)
dev.off()

# 
# jar_data <- sensory_list$cons %>%
#   select(
#     product,
#     jar_strength_of_aftertaste,
#     jar_thickness_of_foam = jar_on_thickness_of_foam,
#     jar_sweetness,
#     jar_strength_of_mint_flavour
#   ) %>%
#   group_by(product) %>%
#   summarise(across(starts_with("jar_"), mean, na.rm = TRUE))

data <- cons_data %>% 
  select(-overall) %>%
  left_join(sens_wide)

p <- inspect_missing_values(data, product)
p

plots <- data %>% 
  mutate(across(c(starts_with("t_num_")), as.character)) %>%
  select(product, starts_with("t_")) %>%
  pivot_longer(starts_with("t_")) %>%
  group_by(product, name) %>%
  mutate(score_jar = dev_from_opt(as.numeric(value), 3),
         score_num = mean(as.numeric(value))) %>%
  ungroup() %>%
  mutate(score = if_else(grepl("t_jar_", name), score_jar, score_num)) %>%
  split(.$name) %>%
  imap(~{
    .x %>%
      mutate(label = sprintf("%s, score = %.3f", product, score)) %>%
      mutate(label = fct_reorder(label, score)) %>%
      group_by(label) %>%
      count(value) %>%
      mutate(rate = n/sum(n)) %>%
      ggplot(aes(value, rate)) +
      geom_bar(stat = "identity") +
      ggtitle(.y) +
      facet_wrap(~label)
  })

plot_file <- file.path(output_path, "plots", str_c(Sys.Date(), " - distr.pdf"))
graphics.off()
pdf(plot_file, width = 8, height = 8)
plots %>%
  walk(print)
dev.off()

# ggsave(file.path('output', 'model', 'plots', 'missing_values.png'), 
#        p, width = 16, height = 12)


all_data_jar <- data %>%
  select(-starts_with("t_num_")) %>%
  pivot_longer(starts_with("t_jar_"), names_to = "Code", values_to = "Target")  %>%
  group_by(Code) %>%
  nest() %>%
  ungroup() %>%
  mutate(type = "jar")
all_data_num <- data %>%
  select(-starts_with("t_jar_")) %>%
  pivot_longer(starts_with("t_num_"), names_to = "Code", values_to = "Target")  %>%
  group_by(Code) %>%
  nest() %>%
  ungroup() %>%
  mutate(type = "num")

with(set.seed(1),
     all_data <- all_data_jar %>%
       bind_rows(all_data_num) %>%
       # inner_join(jar_list) %>%
       # unite(Code, Code, attributes, sep = "-") %>%
       # mutate(data = map2(data, sens_vars,
       #                    ~.x %>% select(product, Target, all_of(.y)))) %>%
       # mutate(data = map2(data, sens_vars,
       #                    ~.x %>% select(product, consid, Target, starts_with("dem"), all_of(.y)))) %>%
       # mutate(data = map(data, mutate, Target = factor(Target, levels = 1:5, ordered = TRUE))) %>%
       # mutate(rsmpl = map(data, vfold_cv, v = 10))
       mutate(cons_cv = map(data, makeCartesianResampling, consid = vfold_cv(v = 3))) %>%
       # mutate(prod_cv = map(data, makeCartesianResampling, product = loo_cv())) %>%
       pivot_longer(ends_with("_cv"), names_to = "cv_type", values_to = "rsmpl") 
)

consumer_variables <- data %>%
  select(starts_with("dem_")) %>%
  colnames()
  
product_variables <- data %>%
  select(starts_with("sens_")) %>%
  colnames()
