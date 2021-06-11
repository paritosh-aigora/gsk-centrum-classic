
# thickening_info <- file.path("input", "imported_data", "gummy_color_imported_data.rds") %>%
#   read_rds() %>%
#   deframe() %>%
#   pluck("sensory") %>%
#   select(Sample_Name, Gelatin, Pectin, Tapioca) %>%
#   mutate(Sample_Name = gsub(" \\(.+\\)", "", Sample_Name)) %>%
#   distinct()

variables_info <- readxl::read_excel("input/match_files/Attribute_selection_JAR.xlsx") %>%
  mutate(variable = janitor::make_clean_names(Codename))

imported_data_path <- file.path(
  "input",
  "imported_data",
  "gummy_imported_data.rds")

sensory_list <- readRDS(imported_data_path) %>% 
  deframe()

# actionable_attributes_path <- file.path(
#   "input",
#   "raw_data",
#   "GSK Centrum actionable attributes.xlsx"
# )
# 
# data_key_path <- file.path(
#   "input",
#   "data_keys",
#   "gummy_data_key.xlsx"
# )
# 
# actionable_attributes <- readxl::read_excel(actionable_attributes_path, sheet = "Gummy attribute list") %>%
#   filter(!is.na(`Actionable?`)) %>%
#   pull(Variable) %>%
#   janitor::make_clean_names()
# 
# selected_attributes <- readxl::read_excel(data_key_path, sheet = "Panel Details") %>%
#   pull(Codename) %>%
#   janitor::make_clean_names()

product_keys <- readxl::read_excel(file.path("input", "data_keys", "gummy_data_key.xlsx"),
                                   "Sample Info") %>%
  select(product = Consumer, sample_name = Panel)


sens_wide <- sensory_list$sensory %>%
  rename_with(janitor::make_clean_names) %>%
  inner_join(product_keys) %>%
  select(
    product,
    all_of(variables_info$variable)
  ) %>% 
  group_by(product) %>% 
  summarize(
    across(everything(), .fns = mean, na.rm = TRUE)) %>%
  rename_with(sprintf, fmt = "sens_%s",
              all_of(variables_info$variable[variables_info$Type == "Sensory"])) %>%
  rename_with(sprintf, fmt = "an_%s", 
              all_of(variables_info$variable[variables_info$Type == "Analytical"])) %>%
  rename_with(sprintf, fmt = "base_%s", 
              all_of(variables_info$variable[variables_info$Type == "Base"]))

cons_data <- sensory_list$cons %>%
  select(
    product = Product,
    consid = ID,
    overall = Overall_Opinion,
    dem_age = `SAMPLE AGE`,
    t_jar_sweetnes = JAR_sweetness,
    t_jar_melting = JAR_melting,
    t_jar_flavor_strength = JAR_flavor_strength,
    t_jar_chewiness = JAR_chewiness,
    t_jar_tartness = JAR_tartness
    # ,
    # t_jar_aroma_intensity = JAR_aroma_intensity,
    # t_jar_flavour_duration = JAR_flavour_duration,
    # t_num_newness = Newness
  ) %>%
  mutate(
    dem_age = case_when(
      dem_age == "1" ~ "18-44",
      dem_age == "2" ~ "45-65",
      T ~ NA_character_
    ),
    across(c(starts_with("t_jar_")),
           factor, levels = 1:5, ordered = TRUE),
    across(c(starts_with("t_num_")),
           as.numeric),
    overall = as.numeric(overall)
  )

tg_plots <- cons_data %>%
  mutate(across(c(starts_with("t_num_")), factor, levels = 1:5, ordered = TRUE)) %>%
  pivot_longer(starts_with("t_")) %>%
  group_by(dem_age, name, value) %>%
  summarise(overall = mean(overall),
            n = n()) %>%
  split(.$name) %>%
  imap(~{
    label <- gsub("t_|num_", "", .y)
    .x %>%
      ggplot(aes(value, overall)) +
      geom_point(aes(color = dem_age), size = 5) +
      ggrepel::geom_text_repel(aes(label = n), min.segment.length = 1) +
      ggthemes::scale_color_tableau() +
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

product_data <- tribble(
  ~Group, ~product_data,
  "sens", sens_wide %>% select(product, starts_with("sens_")),
  "an", sens_wide %>% select(product, starts_with("an_")),
  "an_base", sens_wide %>% select(product, starts_with("an_"), starts_with("base_")),
  "sens_an", sens_wide %>% select(product, starts_with("sens_"), starts_with("an_")),
  "sens_an_base", sens_wide %>% select(product, starts_with("sens_"), starts_with("an_"), starts_with("base_"))
)

data <- cons_data %>% 
  select(-overall) %>%
  left_join(sens_wide)

p <- inspect_missing_values(sens_wide, product)
p

plots <- cons_data %>% 
  mutate(across(c(starts_with("t_num_")), factor, levels = 1:5, ordered = TRUE)) %>%
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

all_data_jar <- cons_data %>%
  select(-overall) %>%
  select(-starts_with("t_num_")) %>%
  pivot_longer(starts_with("t_jar_"), names_to = "Code", values_to = "Target")  %>%
  nest(data = -Code) %>%
  mutate(type = "jar")
# all_data_num <- cons_data %>%
#   select(-overall) %>%
#   select(-starts_with("t_jar_")) %>%
#   pivot_longer(starts_with("t_num_"), names_to = "Code", values_to = "Target")  %>%
#   nest(data = -Code) %>%
#   mutate(type = "num")

with(set.seed(1),
     all_data <- all_data_jar %>%
       # bind_rows(all_data_num) %>%
       inner_join(product_data, by = character()) %>%
       unite(Code, Code, Group, sep = "-") %>%
       mutate(data = map2(data, product_data, inner_join, by = "product")) %>%
       select(-product_data) %>%
       # mutate(data = map2(data, sens_vars,
       #                    ~.x %>% select(product, Target, all_of(.y)))) %>%
       # mutate(data = map2(data, sens_vars,
       #                    ~.x %>% select(product, consid, Target, starts_with("dem"), all_of(.y)))) %>%
       # mutate(data = map(data, mutate, Target = factor(Target, levels = 1:5, ordered = TRUE))) %>%
       # mutate(rsmpl = map(data, vfold_cv, v = 10))
       mutate(cons_cv = map(data, makeCartesianResampling, consid = vfold_cv(v = 10))) %>%
       # mutate(prod_cv = map(data, makeCartesianResampling, product = loo_cv())) %>%
       pivot_longer(ends_with("_cv"), names_to = "cv_type", values_to = "rsmpl") 
)

consumer_variables <- bind_rows(all_data$data) %>%
  select(starts_with("dem_")) %>%
  colnames()
  
product_variables <- bind_rows(all_data$data) %>%
  select(starts_with("sens_"), starts_with("an_"), starts_with("base_")) %>%
  colnames()
