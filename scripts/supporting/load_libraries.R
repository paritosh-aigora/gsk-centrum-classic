# Load packages for project, installing if necessary

load_package <- function(x) {
  for (i in x) {
    #  require returns TRUE invisibly if it was able to load package
    if (!require(i , character.only = TRUE)) {
      #  If package was not able to be loaded then re-install
      install.packages(i , dependencies = TRUE)
      #  Load package after installing
      require(i , character.only = TRUE)
    }
  }
}

#  Then try/install packages...

packages <- c(
  "tidyverse",
  "RVerbalExpressions",
  "purrr",
  "tidyverse",
  "SensoMineR",
  "FactoMineR",
  "factoextra",
  "caret",
  "Amelia",
  "corrgram",
  "GGally",
  "fpc",
  "NbClust",
  "h2o",
  "tidymodels",
  "ROSE",
  "DALEX",
  "breakDown",
  "ranger",
  "kernlab",
  "nnet",
  "openxlsx",
  "extrafont",
  "officer",
  "rvg",
  "scales",
  "patchwork",
  "readxl",
  "glmnet",
  "textclean",
  "mice",
  "tools",
  "ggwordcloud",
  "stats",
  "dendextend",
  "cluster"
)

load_package(packages)
