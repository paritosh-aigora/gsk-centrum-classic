
# initialze h2o, and start it if not already running
tryCatch(
  expr = {
    h2o.init(startH2O = FALSE, nthreads = -1)
  },
  error = function(e) {
    h2o.init()
  },
  finally = {
    
  }
)

print(str_c("Analyzing: ",
            source_stem,
            " ------------------------------"))

# clear h2o memory
h2o.removeAll()

# define output directory (which is also an input directory in this case)
output_dir <- file.path("output", "reporting")

# load data
data_file_path <-
  file.path("input",
            "cleaned_data",
            str_c(source_stem, "_cleaned_data.Rdata"))
load(data_file_path)

xxx <- cleaned_data$x
ddd <- cleaned_data$d
yyy <- cleaned_data$y

# load the model and various information about the model
info_file_path <-
  file.path(
    "output",
    "modeling_results",
    source_stem,
    str_c(source_stem,
          "_selected_model_info.RData")
  )
load(info_file_path)

selected_model <-
  h2o.loadModel(file.path(
    "output",
    "modeling_results",
    source_stem,
    selected_model_info[["model_path"]]
  ))

# identify model features

model_features <- selected_model@parameters$x

# Prepare data set

means_data <- xxx %>%
  group_by(Stim) %>%
  summarize_at(vars(one_of(model_features)), mean, na.rm = TRUE)

# prepare long form as well for joining later with predicted results

means_data_long <- means_data %>%
  pivot_longer(-Stim, names_to = "var", values_to = "actual")

# Prepare respondent data set

cons_data <- yyy %>%
  left_join(ddd)

# load principle component results

pca_list <- readRDS(file.path("output", "data_exploration", source_stem, "pca_list.RData"))

res_pca <- pca_list$res_pca

# convert means to principle components

o_names_vect <- x_names_vect <- cleaned_keys$x %>% 
  deframe()

x_names_vect <- cleaned_keys$x %>% 
  select(var_name, var) %>% 
  deframe()

prcomp_data <- means_data %>% 
  select("Stim", one_of(cleaned_keys$x$var)) %>% 
  set_names(c("Stim", cleaned_keys$x$var_name)) %>% 
  find_comp_profiles(res_pca)

# Determine possible levels of each component

num_poss_levels <- 20

attrib_levels <- prcomp_data %>%
  select(-Stim) %>%
  map(extendrange) %>%
  map(function(x) {
    seq(from = x[[1]],
        to = x[[2]],
        length.out = num_poss_levels)
  })

# create matrix of possible component levels

poss_levels_mat <- attrib_levels %>%
  enframe(name = "var", value = "levels") %>%
  mutate(var = fct_inorder(factor(var))) %>%
  mutate(prcomp_data = list(prcomp_data)) %>%
  unnest(prcomp_data) %>%
  select(Stim, var, levels, everything()) %>%
  arrange(Stim, var) %>%
  unnest(levels) %>%
  mutate(var = as.character(var))

sim_prcomp_data <- poss_levels_mat

row_ind <- 1
for (row_ind in 1:nrow(poss_levels_mat)) {
  sim_prcomp_data[row_ind, poss_levels_mat[row_ind, "var"] %>% as.character()] <-
    poss_levels_mat[row_ind, "levels"]
  
}

# convert possible component levels back to original variables

sim_means_data <- sim_prcomp_data %>% 
  select(Stim, starts_with("PC")) %>% 
  find_orig_profiles(res_pca) %>% 
  select("Stim", one_of(cleaned_keys$x$var_name)) %>% 
  set_names(c("Stim", cleaned_keys$x$var)) %>% 
  mutate(var = sim_prcomp_data$var, levels = sim_prcomp_data$levels) %>% 
  select(Stim, var, levels, everything())

# split data for looping

sim_means_list <- sim_means_data %>%
  split(., .$Stim)

# prepare data for loop

join_data_list <-
  list(cons_data,
       cons_data %>% filter(Cluster == 1),
       cons_data %>% filter(Cluster == 2)) %>%
  set_names(c("All", "Cluster 1", "Cluster 2"))

# predict liking

join_data <- join_data_list[[1]]

pred_stim_liking <- function(join_data, means_data) {
  # predict stimulus means overall
  
  input_data <- join_data %>%
    left_join(means_data)
  
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

pred_stim_means_list <- join_data_list %>%
  map(pred_stim_liking, means_data)

full_dol_list <- vector("list")

join_name <- names(join_data_list)[[1]]

for (join_name in names(join_data_list)) {
  print(join_name)
  
  pred_stim_means <- pred_stim_means_list[[join_name]]
  
  dol_list <- vector("list")
  
  stim_ind <- 1
  
  for (stim_ind in seq_along(sim_means_list)) {
    print(stim_ind)
    sim_means_data_stim <- sim_means_list[[stim_ind]]
    
    input_data <- sim_means_data_stim %>%
      left_join(join_data_list[[join_name]])
    
    input_frame <- input_data %>%
      as.h2o(destination_frame = "input")
    
    pred_res <- input_data %>%
      select(Stim, var, levels) %>%
      mutate(Pred = h2o.predict(selected_model, input_frame) %>% as.vector())
    
    improve_res <- pred_res %>%
      group_by(Stim, var, levels) %>%
      summarize_at(c("Pred"), mean) %>%
      ungroup() %>% 
      mutate(target = as.numeric(pred_stim_means[stim_ind, "Pred"])) %>%
      mutate(delta = Pred - target) %>%
      group_by(var) %>% 
      filter(delta >= 0, delta == max(delta)) %>% 
      slice(1) %>% 
      ungroup() %>% 
      arrange(desc(delta)) %>% 
      left_join(sim_means_data_stim) %>% 
      rename(Component = var, Comp_Val = levels, Opt_Val = Pred, Target_Val = target, Improvement = delta) %>% 
      rename_with(Vectorize(function(x){o_names_vect[[x]]}), starts_with("X"))
    
    dol_list[[stim_ind]] <- improve_res
    
  }
  
  full_dol_list[[join_name]] <- dol_list
  
}

full_dol_list[[1]][[1]] 

# prepare importance information for output

import_info <- selected_model_info[["importance"]]

x_import_mat <- import_info$x %>%
  rename(
    Variable = var_name,
    `Random Forest` = `RF Proportion`,
    `Gradient Boosted Trees` = `GBM Proportion`
  ) %>%
  mutate(Type = "Panel") %>%
  select(Variable, Type, `Random Forest`, `Gradient Boosted Trees`)

d_import_mat <- import_info$d %>%
  rename(
    Variable = var_name,
    `Random Forest` = `RF Proportion`,
    `Gradient Boosted Trees` = `GBM Proportion`
  ) %>%
  mutate(Type = "Respondent") %>%
  select(Variable, Type, `Random Forest`, `Gradient Boosted Trees`)

import_mat <- x_import_mat %>%
  rbind(d_import_mat) %>%
  arrange(desc(`Random Forest`)) %>%
  mutate(Variable = tools::toTitleCase(Variable)) %>%
  list() %>%
  set_names("Variable Importance")

sample_names <- cleaned_keys$s %>%
  pull(Stim_Name) %>%
  as.character() %>%
  set_names(cleaned_keys$s$Stim)

for (join_name in names(full_dol_list)) {
  full_dol_list[[join_name]] <- full_dol_list[[join_name]] %>%
    set_names(sample_names)
}

visualization_info_path <-
  file.path(
    "output",
    "modeling_results",
    source_stem,
    str_c(source_stem, "_visualization_info.RData")
  )

save(import_mat, full_dol_list, file = visualization_info_path)

sheet_names <- "Importance" %>%
  c(
    sample_names[names(sim_means_list)] %>%
      as.character() %>%
      replace_non_ascii() %>%
      str_replace_all(" *- *", " ") %>%
      str_replace_all("/", " ") %>%
      str_trunc(width = 30)
  )

output_list <- c(import_mat, full_dol_list[[1]])

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

sheet_ind <- 1
for (sheet_ind in seq_along(output_list)) {
  worksheet_data <- output_list[[sheet_ind]]
  sheet_name <- sheet_names[[sheet_ind]]
  
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

# Write results out to Excel

output_file <-
  str_c(Sys.Date(), " - ", source_label, " - ML Output.xlsx")

output_file_path_wb <- file.path(output_dir, output_file)

saveWorkbook(wb, file = output_file_path_wb, overwrite = TRUE)

# save results to file for later use

output_file_path_data <-
  file.path(
    "output",
    "modeling_results",
    source_stem,
    "lo_res.rds"
  )

saveRDS(output_list, file = output_file_path_data)
