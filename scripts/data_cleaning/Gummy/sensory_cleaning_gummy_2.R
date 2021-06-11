library(tidyverse)
library(stringr)
library(openxlsx)
#
file_name <- 'RH03575_CentrumGummy_Sensory.csv'

input_file_path <- file.path("input","raw_data",file_name)


matching_file <- file.path("input", "match_files","match_file.xlsx") %>% 
  read.xlsx(sheet = 2)


raw_data  <- read_csv(input_file_path)

modified_data <- raw_data %>%
  select("Test_Name","Blinding_Code","Sample_Set_Number","Session_Number","Sample_Name","Panelist_Name",starts_with("Q")) %>%
  set_names(str_remove(colnames(.), "^Q\\d_\\d{1,2}__|^Q\\d__")) %>% 
  mutate(temp = Sample_Name %>% str_split(pattern = fixed("(")) %>% map(str_remove, fixed(")"))) %>% 
  mutate(Product_name = trimws(map(temp,1), which = "both")) %>% 
  mutate(Variant = trimws(map(temp,2), which = "both"))



sens_data <- modified_data %>% 
  select(-c("Test_Name","Sample_Set_Number","Blinding_Code","temp")) %>% 
  group_by(Sample_Name) %>% 
  select_if(is.numeric) %>% 
  ungroup() %>% 
  mutate(Sample_Name = trimws(unlist(map(str_split(Sample_Name, pattern = fixed("(")), function(l) l[[1]])), which = "both")) %>% 
  mutate(Sample_Name = str_replace_all(string = Sample_Name,pattern = "Smarty Pants", replacement = "SmartyPants")) %>% 
  group_by(Panelist_Name, Sample_Name, Session_Number) %>% 
  summarize_all(mean) %>% 
  ungroup()

 
  #drop_na() 
  # summarize_all(mean) %>% 
  # left_join(matching_file, by = c("Sample_Name" = "Sensory")) %>% 
  # select(-Consumer) %>%
  # select(Sensory, everything())

  
