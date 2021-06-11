library(tidyverse)
library(openxlsx)
library(janitor)
library(readxl)

file_name <- "Sample data format combined.xlsx"

input_file_path <- file.path("input", "raw_data", "RH03561_Centrum Category Appraisal (Gummy)_Conusmer", file_name)

analytical_data <- input_file_path %>%
  read.xlsx() %>% 
  select(-Units) %>% 
  group_by(Product_Name, Analytical_Attribute) %>% 
  summarize_at(vars(Analytical_Reading), mean) %>% 
  pivot_wider(id_cols = Product_Name, names_from = "Analytical_Attribute", values_from = "Analytical_Reading") %>% 
  ungroup()

matching_name <- "prod_match_2.xlsx"

matching_file_path <- file.path("input","match_files",matching_name)
product_matching <- matching_file_path %>% 
  read.xlsx()

analytical_data <- analytical_data %>% 
  left_join(product_matching, by = c("Product_Name" = "Analytical_name"))

analytical_data <- analytical_data %>% 
  select(Sensory_name, everything()) %>% 
  select(-Product_Name) 

analytical_data <- analytical_data %>% 
  rename(Product_Name = Sensory_name)
