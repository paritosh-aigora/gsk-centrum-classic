
# build prediction engine

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

pred_stim_liking <- function(cons_data, means_data) {
  # predict stimulus means overall
  
  input_data <- cons_data %>%
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

pred_table <- pred_stim_liking(cons_data, means_data)

comparison_table <- cons_data %>% 
  group_by(Stim) %>% 
  summarize(Actual = mean(Y, na.rm = TRUE)) %>% 
  left_join(pred_table) %>% 
  left_join(cleaned_keys$s) %>% 
  select(Stim_Name, Actual, Pred) 
