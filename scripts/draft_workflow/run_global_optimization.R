
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

# for each sample, predict liking of current profile, then search for best adjusted level of each attribute used in the chosen model

#for (source_stem in source_stems_to_run) {
print(str_c(
  "Analyzing: ",
  source_stem,
  " ------------------------------"
))

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
    str_c(
      source_stem,
      "_selected_model_info.RData"
    )
  )
load(info_file_path)

selected_model <-
  h2o.loadModel(file.path("output", "modeling_results", source_stem, selected_model_info[["model_path"]]))

# identify model features

model_features <- selected_model@parameters$x

# create function that finds global optimum for given consumer and profile data

find_global_optimum <- function(cons_data, profile_data, seed = 1234){
  
  # set seed
  
  set.seed(seed)
  
  # Determine possible levels of each attribute
  
  num_poss_levels <- 20
  
  attrib_levels <- profile_data %>%
    select(-Stim) %>%
    map(extendrange) %>%
    map_dfc(function(x) {
      seq(from = x[[1]],
          to = x[[2]],
          length.out = num_poss_levels)
    })
  
  
  # for this application, the population data are fixed by the consumer data to create local means computation
  
  predict_pop_liking <- predict_pop_liking_full %>% 
    partial(pop_data = cons_data, selected_model = selected_model)
  
  num_vars <- attrib_levels %>% 
    ncol()
  
  prof1 <- profile_data[1, -1]
  prof2 <- profile_data[2, -1]
  mut_rate <- 0.1
  
  breed_profiles_full <- function(prof1, prof2, mut_rate, attrib_levels, num_vars, num_poss_levels){
    
    # breed without mutation
    daughter_profile <- prof1
    
    prof2_flags <- sample(c(FALSE, TRUE), size = num_vars, replace = TRUE)
    if (any(prof2_flags)) daughter_profile[prof2_flags] <- prof2[prof2_flags]
    
    # allow mutation
    mut_flags <- sample(c(FALSE, TRUE), size = num_vars, replace = TRUE, prob = c(1-mut_rate, mut_rate))
    
    if (any(mut_flags)){
      
      # prepare for mutations
      num_muts <- mut_flags %>% 
        sum()
      
      attrib_level_inds <- sample(num_poss_levels, num_muts, replace = TRUE)
      
      mut_inds <- mut_flags %>% 
        which()
      
      # conduct mutations
      i <- 1
      for (i in seq_along(mut_inds)){
        
        mut_ind <- mut_inds[[i]]
        attrib_level_ind <- attrib_level_inds[[i]]
        
        daughter_profile[mut_ind] <- attrib_levels[attrib_level_ind, mut_ind]
        
      }
      
    }
    
    return(daughter_profile)
    
  }
  
  # create local profile breeding function
  breed_profiles <- partial(breed_profiles_full, mut_rate = mut_rate, attrib_levels = attrib_levels, num_vars = ncol(attrib_levels), num_poss_levels = nrow(attrib_levels))
  
  #breed_profiles(prof1, prof2)
  
  # create function to generate a new generation from an existing sample
  
  means_data <- profile_data
  num_total <- 1000
  
  num_current <- means_data %>% 
    nrow()
  num_needed <- num_total - num_current
  
  poss_combs <- combn(num_current, 2)
  num_poss_combs <- ncol(poss_combs)
  
  create_new_generation_full <- function(means_data, num_total, poss_combs, num_poss_combs){
    
    num_current <- means_data %>% 
      nrow()
    num_needed <- num_total - num_current
    
    which_combs <- sample(num_poss_combs, num_needed, replace = TRUE)
    
    selected_combs <- poss_combs[, which_combs] %>% 
      as_tibble()
    
    selected_comb <- selected_combs[[1]]
    
    breed_local <- function(selected_comb){
      
      ind1 <- selected_comb[[1]]
      ind2 <- selected_comb[[2]]
      
      output <- breed_profiles(means_data[ind1, -1], means_data[ind2, -1])
      
      return(output)
      
    }
    
    new_means_data <- selected_combs %>% 
      map_dfr(breed_local) %>% 
      mutate(Stim = num_current + 1:nrow(.)) %>% 
      select(Stim, everything())
    
    new_generation <- means_data %>% 
      bind_rows(new_means_data)
    
    return(new_generation)
    
  }
  
  create_new_generation <- partial(create_new_generation_full, num_total = num_total, poss_combs = poss_combs, num_poss_combs = num_poss_combs)
  
  # run through multiple generations, searching for best combinations
  
  means_data <- profile_data
  
  num_top <- 40
  num_other <- 10
  num_generations <- 10
  
  i <- 1
  for (i in 1:num_generations){
    
    new_generation <- create_new_generation(means_data)
    
    generation_res <- new_generation %>% 
      predict_pop_liking() %>% 
      arrange(desc(Pred))
    
    print(generation_res)
    
    # keep top solutions along with a a few other solutions at random, and breed
    
    top_inds <- generation_res %>% 
      slice(1:num_top) %>% 
      pull(Stim)
    
    other_inds <- generation_res %>% 
      filter(!(Stim %in% top_inds)) %>% 
      slice_sample(n = num_other) %>% 
      pull(Stim)
    
    
    keep_inds <- c(top_inds, other_inds)
    
    means_data <- new_generation[keep_inds,] %>% 
      mutate(Stim = 1:nrow(.))
    
  }
  
  global_opt <- means_data[1, ]
  
  return(global_opt)
  
}

# Prepare data set

profile_data <- xxx %>%
  group_by(Stim) %>%
  summarize_at(vars(one_of(model_features)), mean, na.rm = TRUE) %>% 
  mutate(Stim = as.numeric(Stim))

# Prepare respondent data set

cons_data <- ddd 

# search for optimal solutions

cluster_cons_data <- cons_data %>% 
  split(.$Cluster) %>% 
  set_names(str_c("Cluster ", names(.)))

global_opt_list <- list("All" = cons_data) %>% 
  c(cluster_cons_data) %>% 
  map(find_global_optimum, profile_data = profile_data)

# save results
output_file_path <-
  file.path("output", "modeling_results", source_stem,
            "global_opt_list.RDS")

saveRDS(global_opt_list, output_file_path)



