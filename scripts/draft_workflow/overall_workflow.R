
# overall workflow -------------------------------------------------------

# set directory paths
workflow_path <- file.path("scripts", "draft_workflow")

# load functions and constants
source(file.path("scripts", "supporting", "load_libraries.R"))
source(file.path("scripts", "supporting", "load_functions.R"))
source(file.path("scripts", "supporting", "load_constants_gsk.R"))

# register fonts if necessary (only need to run once per r version update)
# library(extrafontdb)
# loadfonts()

# define stem referring to specific dataset to be analyzed
 

#source_stem <- "tablet"
#source_stem <- "tablet(18-35)"
#source_stem <- "tablet(36-66)"
source_stem <- "gummy"
#source_label <- "Tablet"
#source_label <- "Tablet(18-35)"
#source_label <- "Tablet(36-66)"
source_label <- "Gummy"
# select target variable
target_variable <- "Y"

# specify whether or not to allow for user-specified clusters
user_clusters <- FALSE

# specify whether or not to remove bouquet attributes
remove_b <- FALSE

# prepare data for analysis
source(file.path(workflow_path, "prep_data.R")) # need data_key for this step

# condition data for modeling and make plots
source(file.path(workflow_path, "condition_data.R"))

# fit drivers of liking model
source(file.path(workflow_path, "fit_dol_model.R"))

# fit cluster characterization
#source(file.path(workflow_path, "fit_clustering_model.R"))

# # run local optimizations
source(file.path(workflow_path, "run_local_optimization.R"))

# make local optimization charts
source(file.path(workflow_path, "make_local_opt_charts.R"))

# run global optimization
source(file.path(workflow_path, "run_global_optimization.R"))

# make global optimization charts
source(file.path(workflow_path, "make_global_opt_charts.R"))

# make report
source(file.path(workflow_path, "make_report.R"))

# bonus: a tool to predict liking from consumer and profile data (should customize before using)
#source(file.path(workflow_path, "predict_stim_liking.R"))



