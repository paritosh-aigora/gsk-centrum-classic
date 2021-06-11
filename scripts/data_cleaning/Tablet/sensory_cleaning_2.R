library(tidyverse)
library(openxlsx)

file_name <- 'RH03575_CentrumTablets_Sensory.csv'
 
input_file_path <- file.path("input","raw_data",file_name)

matching_file <- file.path("input", "match_files","match_file.xlsx") %>% 
  read.xlsx(sheet = 1)

raw_data  <- read_csv(input_file_path)

#Change
modified_data <- raw_data %>%
  select("Test_Name","Sample_Set_Number","Session_Number","Sample_Name","Unique_Panelist_ID","Blinding_Code",starts_with("Q")) %>% 
  select(str_subset(colnames(.),pattern = ("_Time_Stamp"), negate = TRUE)) %>% 
  set_names(str_remove(colnames(.), "^Q\\d_\\d__|^Q\\d__"))

sens_data <- modified_data %>% 
  select(-c("Test_Name","Sample_Set_Number","Blinding_Code")) %>% 
  left_join(matching_file, by = c("Sample_Name" = "Sensory")) %>% 
  select(-Sample_Name, -Consumer,-Products) %>%
  rename("Panelist"="Unique_Panelist_ID", "Product" = "Product_name" , "Session"="Session_Number") %>% 
  select(-starts_with("Off-Note")) %>% 
  select(Panelist, Product, Session, everything()) %>% 
  filter(Product != "Garden of life_women")

  
  
# colnames(sens_data) %>% 
#   str_replace_all("_"," ") %>% 
#   str_to_title() %>% 
#   clipr::write_clip()

saveRDS(sens_data, file = "output/sensory_data_tablet.rds")
