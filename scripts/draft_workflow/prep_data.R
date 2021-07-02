

# Prepare data for analysis -----------------------------------------------
# This file will be specific to the data source for the time being
# For now we write the script to be as general as possible

# load imported data
# Note: we assume that the data has already been roughly imported
imported_data <-
  read_rds(file.path(
    "input",
    "imported_data",
    str_c(source_stem, "_imported_data.rds")
  ))

imported_data_list <- imported_data %>%
  deframe()

# Read data info (note: prepared by hand) ----

data_info_path <- file.path("input", "data_keys", str_c(source_stem, "_data_key.xlsx"))

data_info_list <- data_info_path %>% 
  read_excel_list()

# sample information

sample_key <- data_info_list$`Sample Info` %>% 
  rename(cons_code = Consumer, panel_code = Panel, gen_code = Report)

# create sample key object ----

s_key <- sample_key %>%
  mutate(Stim_Name = as.factor(gen_code)) %>%
  mutate(Stim = as.factor(as.numeric(Stim_Name))) %>%
  select(Stim, Stim_Name) %>%
  arrange(Stim)

# panel information

panel_info <- data_info_list$`Panel Info`

panel_tab <- panel_info$`Panel Tab`[[1]]
panelist_var <- panel_info$`Panelist Codename`[[1]]
product_var <- panel_info$`Product Codename`[[1]]
session_var <- panel_info$`Session Codename`[[1]]

# panel details

panel_details <- data_info_list$`Panel Details`

# extract panel data into input matrix for machine learning

# load bouquet info (CUSTOM for STEP)

if (remove_b){
  b_key <- readxl::read_excel(file.path("input", "data_keys", "bouquet_key.xlsx"))
}

# extract panel data ----

raw_panel_data <- imported_data_list[[panel_tab]] %>%
  rename(panel_code = !!product_var) %>%
  inner_join(sample_key) %>% 
  rename(Panelist = !!panelist_var, Product = panel_code, Session = !!session_var) %>%
  arrange(Panelist, Product) 

num_panelists <- raw_panel_data %>% 
  pull(Panelist) %>% 
  unique() %>% 
  length()

num_prods <- raw_panel_data %>% 
  pull(Product) %>% 
  unique() %>% 
  length()

header_x_vars <- c("Panelist", "Session", "gen_code")

if (remove_b){
  pre_x_vars <- setdiff(panel_details$Codename, b_key$Codename)
} else {
  pre_x_vars <- panel_details$Codename
}

# prepare x dataset
pre_x_data <- raw_panel_data %>%
  select(one_of(header_x_vars), one_of(pre_x_vars)) %>%
  mutate(Stim = as.numeric(as.factor(gen_code))) %>%
  select(-gen_code) %>%
  rename(ID_X = Panelist) %>%
  mutate_at(c("Session", "Stim"), as.factor) %>%
  select(ID_X, Session, Stim, everything()) %>%
  arrange(ID_X, Session, Stim) %>% 
  pivot_longer(one_of(pre_x_vars), names_to = "Codename", values_to = "Value") %>% 
  left_join(select(panel_details, -Cluster, -Min_val,-Max_val)) %>% 
  select(-Codename) %>% 
  pivot_wider(names_from = "Variable", values_from = "Value")

# temp <- raw_panel_data %>%
#   select(one_of(header_x_vars), one_of(pre_x_vars)) %>%
#   mutate(Stim = as.numeric(as.factor(gen_code))) %>%
#   select(-gen_code) %>%
#   rename(ID_X = Panelist) %>%
#   mutate_at(c("Session", "Stim"), as.factor) %>%
#   select(ID_X, Session, Stim, everything()) %>%
#   arrange(ID_X, Session, Stim) %>% 
#   pivot_longer(one_of(pre_x_vars), names_to = "Codename", values_to = "Value") %>% 
#   left_join(select(panel_details, -Cluster, -Min_val, -Max_val)) %>% 
#   select(-Codename) %>% 
#   pivot_wider(names_from = "Variable", values_from = "Value")

# set key for x dataset
x_key <- pre_x_data %>%
  names() %>%
  .[4:length(.)] %>%
  enframe(name = "var", value = "var_name") %>% 
  mutate(var = str_c("X", var))

attribute_x_vars <- x_key %>% 
  pull(var_name)

# finalize x dataset

x_data <- pre_x_data %>%
  mutate(ID_X = as.factor(as.numeric(as.factor(ID_X)))) %>%
  set_names(c(names(.)[1:3], x_key[["var"]])) %>% 
  mutate_at(vars(starts_with("X")), as.numeric)

# respondent information

respondent_info <- data_info_list$`Respondent Info`

consumer_tab <- respondent_info$`Respondent Tab`[[1]]
sample_var <- respondent_info$`Sample Codename`[[1]]
respondent_var <- respondent_info$`Respondent Codename`[[1]]
liking_var <- respondent_info$`Liking Codename`[[1]]
bad_sample_vals <- respondent_info$`Bad Sample Values`[[1]]
cata_sep <- respondent_info$`CATA Separator`[[1]]

# demographic variable information

resp_details <- data_info_list$`Respondent Details`

# extract consumer hedonic and demographic data ---
# Note: This is always in long form

raw_cons_data <-
  imported_data_list[[consumer_tab]] %>%
  rename(Sample = !!sample_var) %>%
  when(is.null(bad_sample_vals) ~ ., ~filter(., !(Sample %in% bad_sample_vals))) %>% 
  mutate(cons_code = Sample) %>%
  inner_join(sample_key)

#cons_data[is.na(cons_data)] <- 0

# begin to extract demographic variables and make d_key

pre_d_key <- resp_details %>% 
  select(Codename, Variable, Type) %>% 
  unique() %>% 
  mutate(var = str_c("D", 1:nrow(.)))
    
# error check on input file

unique_check_df <- pre_d_key %>% 
  count(Codename, Variable) %>% 
  select(-n)

unique_check_code <- unique_check_df %>% 
  count(Codename) %>% 
  pull(n) %>% 
  max()

unique_check_var <- unique_check_df %>% 
  count(Variable) %>% 
  pull(n) %>% 
  max()

unique_check <- (unique_check_code == 1) & (unique_check_var == 1)

if (!unique_check) stop("There should be a unique correspondence between each Codename and Variable")

 # continue extracting demographic variables and create d_key

pre_demo_vars <- pre_d_key %>% 
  pull(Codename)

demo_var_names <- pre_d_key %>% 
  pull(Variable) 

d_key <- tibble(var = str_c("D", seq_along(demo_var_names)), var_name = demo_var_names)

demo_vars <- d_key %>% 
  pull(var)

# account for possible data reading inconsistency
pre_demo_vars <- pre_demo_vars %>% 
  str_replace_all("\\\\r", "\\\r") %>% 
  str_replace_all("\\\\n", "\\\n")

# extract liking data into target matrix, and extract demographic information into input matrix, for machine learning

cons_prepped_data <- raw_cons_data %>%
  select(one_of(respondent_var),
         one_of(pre_demo_vars),
         !!sym(liking_var),
         gen_code) %>%
  set_names(c("ID_Y", demo_vars, "Y", "Stim")) %>%
  select(ID_Y, Stim, one_of(demo_vars), Y) %>%
  mutate_at(c("ID_Y", "Stim"), compose(as.factor, as.numeric, as.factor)) %>%
  arrange(ID_Y, Stim) %>%  
  mutate(Y = as.numeric(Y)) # in case Y is not numeric for some reason

# refine demographic dataset

pre_d_data <- cons_prepped_data %>%
  select(ID_Y, one_of(demo_vars))

# determine variable types

demo_types <- pre_d_key %>% 
  select(Variable, Type) %>% 
  unique() %>% 
  pull(Type)


# process jar variables (if any)
# TODO: Update with one-hot encoding

if (any(demo_types == "JAR")){
  
  jar_vars <- str_c("D", which(demo_types == "JAR"))
  
  pre_jar_data <- pre_d_data %>% 
    mutate_at(vars(-ID_Y), function(x){ifelse(x == "No Response", NA, x)}) %>%  
    mutate_at(vars(-ID_Y), as.numeric) %>%  
    group_by(ID_Y) %>% 
    summarize_at(vars(jar_vars), mean, na.rm = TRUE) %>% 
    ungroup() 
  
  # remove_vars <- pre_jar_data %>% 
  #   select(-ID_Y) %>% 
  #   nearZeroVar(names = TRUE)
  # 
  # pre_jar_data <- pre_jar_data %>% 
  #   select(-one_of(remove_vars))
  
  jar_data <- pre_jar_data %>% 
    mutate_at(vars(jar_vars), function(x){x >= median(x)}) %>% 
    mutate_at(vars(jar_vars), factor, levels = c(TRUE, FALSE), labels = c("Wants Less", "Wants More")) 
  
  jar_factor_levels <- jar_data %>% 
    select(starts_with("D")) %>% 
    map(levels) %>% 
    map(~tibble(level = ., label = .))
  
} 

# process factor variables

if (any(demo_types == "Factor")){

  factor_vars <- str_c("D", which(demo_types == "Factor"))
  
  factor_levels <- resp_details %>% 
    filter(Type == "Factor") %>% 
    select(Variable, Code, Label) %>% 
    left_join(d_key, by = c("Variable" = "var_name")) %>% 
    select(var, level = Code, label = Label) %>% 
    group_by(var) %>% 
    nest() %>% 
    deframe()
  
  pre_factor_data <- pre_d_data %>% 
    select(ID_Y, one_of(factor_vars)) %>% 
    mutate_at(vars(factor_vars), as.factor)
  
  if (any(demo_types == "JAR")){
    
    factor_vars <- c(factor_vars, str_c("D", which(demo_types == "JAR"))) %>% 
      unique()
    
    factor_levels <- c(factor_levels, jar_factor_levels)
    
    pre_factor_data <- pre_factor_data %>% 
      left_join(jar_data)
    
  }
  
  factor_var <- factor_vars[[1]]
  for (factor_var in factor_vars){
    
    factor_info <- factor_levels[[factor_var]]
    
    pre_factor_data[[factor_var]] <- factor(pre_factor_data[[factor_var]], levels = factor_info$level, labels = factor_info$label) 
  
  }
  
  raw_factor_data <- pre_factor_data %>% 
    unique() %>% 
    mutate_at(vars(-ID_Y), as.character) %>% 
    pivot_longer(-ID_Y, names_to = "var", values_to = "label") %>% 
    left_join(d_key) %>% 
    group_by(var) %>% 
    mutate(label_ind = as.numeric(as.factor(label))) %>%
    ungroup() %>% 
    arrange(ID_Y, var, label_ind) %>% 
    mutate(new_var = str_c(var, "-", label_ind)) %>% 
    mutate(new_var_name = str_c(var_name, ": ", label)) %>% 
    select(ID_Y, var_name, new_var_name, new_var, label) %>% 
    filter(!is.na(new_var))
  
  factor_key_oh <- raw_factor_data %>% 
    distinct(var_name, new_var_name) %>% 
    arrange(new_var_name)
  
  factor_key <- raw_factor_data %>% 
    select(var = new_var, var_name = new_var_name) %>% 
    unique() 
  
  factor_data <- raw_factor_data %>% 
    select(ID_Y, new_var) %>% 
    mutate(value = 1) %>% 
    pivot_wider(names_from = new_var, values_from = value, values_fill = 0)
  
} else {
  
  factor_data <- NULL
  factor_key <- NULL
  factor_key_oh <- NULL
  
}


# process numeric variables (if any)

if (any(demo_types == "Numeric")){
  
  num_vars <- str_c("D", which(demo_types == "Numeric"))
  
  pre_num_data <- pre_d_data %>% 
    select(ID_Y, one_of(num_vars)) %>% 
    unique() 
  
  remove_vars <- pre_num_data %>% 
    select(-ID_Y) %>% 
    nearZeroVar(names = TRUE)
  
  raw_num_data <- pre_num_data %>% 
    mutate_at(vars(num_vars), function(x){x >= median(x)}) %>% 
    mutate_at(vars(num_vars), factor, levels = c(TRUE, FALSE), labels = c("High", "Low"))  %>% 
    unique() %>% 
    mutate_at(vars(-ID_Y), as.character) %>% 
    pivot_longer(-ID_Y, names_to = "var", values_to = "label") %>% 
    left_join(d_key) %>% 
    group_by(var) %>% 
    mutate(label_ind = as.numeric(as.factor(label))) %>%
    ungroup() %>% 
    mutate(new_var = str_c(var, "-", label_ind)) %>% 
    mutate(new_var_name = str_c(var_name, ": ", label)) %>% 
    select(ID_Y, var_name, new_var_name, new_var, label) %>% 
    filter(!is.na(new_var))
  
  num_key_oh <- raw_num_data %>% 
    distinct(var_name, new_var_name) %>% 
    arrange(new_var_name)
  
  num_key <- raw_num_data %>% 
    select(var = new_var, var_name = new_var_name) %>% 
    unique() 
  
  num_data <- raw_num_data %>% 
    select(ID_Y, new_var) %>% 
    mutate(value = 1) %>% 
    pivot_wider(names_from = new_var, values_from = value, values_fill = 0) 
  
} else {
  
  num_data <- NULL
  num_key <- NULL
  num_key_oh <- NULL
  
}

# process CATA variables  (if any)
# we assume the same separator, which is specified in the data key

if (any(demo_types == "CATA")){
  
  cata_vars <- str_c("D", which(demo_types == "CATA"))
  
  cata_info <- resp_details %>% 
    filter(Type == "CATA") %>% 
    select(Variable, Code, Label) %>% 
    left_join(d_key, by = c("Variable" = "var_name")) %>% 
    select(var, var_name = Variable, level = Code, label = Label) %>% 
    mutate_all(as.character)
  
  cata_levels <- cata_info %>% 
    group_by(var) %>% 
    nest() %>% 
    deframe()
  
  pre_cata_data <- pre_d_data %>% 
    select(ID_Y, one_of(cata_vars)) %>% 
    unique() %>% 
    pivot_longer(one_of(cata_vars), names_to = "var", values_to = "temp") %>% 
    mutate(level = str_split(temp, pattern = cata_sep)) %>% 
      select(-temp) %>% 
      unnest(level) %>% 
      left_join(cata_info)

  # # inspect results to potentially revise data key
  # pre_cata_data %>% 
  #   select(ID_Y, Variable, label) %>% 
  #   unique() %>% 
  #   count(Variable, label) %>% 
  #   View()
  
  raw_cata_data <- pre_cata_data %>% 
    select(ID_Y, var_name, var, label) %>% 
    unique() %>% 
    group_by(var) %>% 
    mutate(label_ind = as.numeric(as.factor(label))) %>%
    ungroup() %>% 
    mutate(new_var = str_c(var, "-", label_ind)) %>% 
    mutate(new_var_name = str_c(var_name, ": ", label)) %>% 
    select(ID_Y, var_name, new_var_name, new_var, label)
    
  cata_key_oh <- raw_cata_data %>% 
    distinct(var_name, new_var_name) %>% 
    arrange(new_var_name)
  
  cata_key <- raw_cata_data %>% 
    select(var = new_var, var_name = new_var_name) %>% 
    unique()
  
  cata_data <- raw_cata_data %>% 
    select(ID_Y, new_var) %>% 
    mutate(value = 1) %>% 
    pivot_wider(names_from = new_var, values_from = value, values_fill = 0)
  
} else {
  
  cata_data <- NULL
  cata_key <- NULL
  cata_key_oh <- NULL
  
}

# create final d dataset

pre_d_data <- list(factor_data, cata_data, num_data) %>% 
  .[!map_lgl(., is.null)] %>% 
  reduce(left_join)

# clean up names and keys

pre_d_names <- pre_d_data %>% 
  select(-ID_Y) %>% 
  names()

pre_d_key <- list(d_key, factor_key, cata_key, num_key) %>% 
  .[!map_lgl(., is.null)] %>% 
  reduce(bind_rows) %>% 
  filter(var %in% pre_d_names) %>% 
  deframe() %>% 
  .[pre_d_names] %>% 
  enframe(name = "var", value = "var_name")
  
d_data <- pre_d_data %>% 
  set_names(c("ID_Y", str_c("D", 1:(ncol(.)-1))))

d_key <- pre_d_key %>% 
  mutate(new_var = str_c("D", row_number())) %>% 
  select(var = new_var, var_name)

d_key_oh <- list(factor_key_oh, num_key_oh, cata_key_oh) %>% 
  .[!map_lgl(., is.null)] %>% 
  reduce(bind_rows)

# define hedonic (target) dataset and keys

y_data <- cons_prepped_data %>%
  select(ID_Y, Stim, Y)

y_key <- "Liking" %>%
  enframe(name = "var", value = "var_name") %>%
  mutate(var = "Y")

# save data and keys

prepped_data <- list(x = x_data, d = d_data, y = y_data)
prepped_keys <- list(x = x_key,
                     d = d_key,
                     y = y_key,
                     s = s_key, 
                     oh = d_key_oh)

save(
  prepped_data,
  prepped_keys,
  file = file.path("input", "prepped_data", str_c(source_stem, "_prepped_data.RData"))
)


