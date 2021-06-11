library(tidyverse)
library(openxlsx)
library(stringr)
#
file_name <- 'RH03575_CentrumGummy_Sensory.csv'

input_file_path <- file.path("input","raw_data",file_name)


# matching_file <- file.path("input", "match_files","match_file.xlsx") %>% 
#   read.xlsx(sheet = 2)

gummy_ingredients <- file.path("input", "match_files","gummy_ingredients.xlsx") %>% 
  read.xlsx(sheet = 1)

raw_data  <- read_csv(input_file_path)

modified_data <- raw_data %>%
  select("Test_Name","Blinding_Code","Sample_Set_Number","Session_Number","Sample_Name","Panelist_Name",starts_with("Q")) %>%
  set_names(str_remove(colnames(.), "^Q\\d_\\d{1,2}__|^Q\\d__")) %>% 
  mutate(temp = Sample_Name %>% str_split(pattern = fixed("(")) %>% map(str_remove, fixed(")"))) %>% 
  mutate(Product_name = trimws(map(temp,1), which = "both")) %>% 
  mutate(Variant = trimws(map(temp,2), which = "both"))



color_data <- modified_data %>% 
  select(-c("Test_Name","Sample_Set_Number","Blinding_Code","temp")) %>% 
  group_by(Sample_Name) %>% 
  select_if(is.numeric) %>% 
  ungroup() %>% 
  #mutate(Sample_Name = trimws(unlist(map(str_split(Sample_Name, pattern = fixed("(")), function(l) l[[1]])), which = "both")) %>% 
  #mutate(Sample_Name = str_replace_all(string = Sample_Name,pattern = "Smarty Pants", replacement = "SmartyPants")) %>% 
  #filter(Sample_Name != "SmartyPants Women's Formula (Orange Gummy)") %>% 
  drop_na() 
  #left_join(gummy_ingredients, by = c("Sample_Name" = "Sample_name"))

relevance_data <- color_data 

relevance_data <- relevance_data %>% 
  pivot_longer(cols = (!Sample_Name & !Session_Number & !Panelist_Name), names_to = "variable", values_to = 'value') %>% 
  select(-Panelist_Name, -Session_Number)
#drop_na() 
# summarize_all(mean) %>% 
# left_join(matching_file, by = c("Sample_Name" = "Sensory")) %>% 
# select(-Consumer) %>%
# select(Sensory, everything())




input_file_name <- "relevance_key.xlsx"

aroma <-  read.xlsx(file.path("input", "match_files", input_file_name), sheet = "Relevant_aroma")
flavor <-  read.xlsx(file.path("input", "match_files", input_file_name), sheet = "Relevant_flavor")
afterflavor <- read.xlsx(file.path("input", "match_files", input_file_name), sheet = "Relevant_afterflavor")

relevant_aroma <- aroma %>% 
  pivot_longer(cols= !Panel, names_to = 'variable',values_to = 'existence') %>% 
  inner_join(relevance_data, by = c("Panel" = "Sample_Name","variable" = "variable")) %>% 
  filter(existence == 1) %>% 
  select(-existence) %>% 
  group_by(Panel) %>% 
  summarize_at(vars(value),mean) %>% 
  rename(relevant_aroma = value)


relevant_flavor <- flavor %>% 
  pivot_longer(cols= !Panel, names_to = 'variable',values_to = 'existence') %>% 
  left_join(relevance_data, by = c("Panel" = "Sample_Name","variable" = "variable")) %>% 
  filter(existence == 1) %>% 
  select(-existence) %>% 
  group_by(Panel) %>% 
  summarize_at(vars(value),mean) %>% 
  rename(relevant_flavor = value)

relevant_afterflavor <- afterflavor %>% 
  pivot_longer(cols= !Panel, names_to = 'variable',values_to = 'existence') %>% 
  left_join(relevance_data, by = c("Panel" = "Sample_Name","variable" = "variable")) %>% 
  filter(existence == 1) %>% 
  select(-existence) %>% 
  group_by(Panel) %>% 
  summarize_at(vars(value),mean) %>% 
  rename(relevant_afterflavor = value)



relevant_attributes <- relevant_aroma %>% 
  left_join(relevant_flavor) %>% 
  left_join(relevant_afterflavor) %>% 
  mutate(temp = Panel %>% str_split(pattern = fixed("(")) %>% map(str_remove, fixed(")"))) %>% 
  mutate(Product_name = trimws(map(temp,1), which = "both")) %>% 
  mutate(Variant = trimws(map(temp,2), which = "both")) %>% 
  select(-temp,-Variant,-Panel) %>% 
  mutate(Product_name = str_replace_all(string = Product_name,pattern = "Smarty Pants", replacement = "SmartyPants")) %>% 
  rename(Panel = Product_name) %>% 
  select(Panel, everything()) %>% 
  group_by(Panel) %>% 
  summarize_all(mean)

# relevant_attributes <- relevant_attributes %>% 
#   left_join(gummy_ingredients, by = c("Panel" = "Product"))
  

source(file.path("scripts","data_cleaning","Gummy","sensory_cleaning_gummy_2.R"))

sens_data <- sens_data %>% 
  left_join(relevant_attributes, by = c("Sample_Name" = "Panel"))


source(file.path("scripts","data_cleaning","Gummy","analytical_cleaning_gummy.R"))

sens_data <- sens_data %>% 
  left_join(analytical_data, by = c("Sample_Name" = "Product_Name"))
