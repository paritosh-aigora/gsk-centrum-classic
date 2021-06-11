library(tidyverse)
library(openxlsx)
library(janitor)
library(readxl)

file_name <- "Tablet_consumer.csv"

input_file_path <- file.path("input", "raw_data", "RH03561_Centrum Category Apprasial (Tablet)_Consumer", file_name)

column_matching <- file.path("input", "match_files", "tablet_column_name.xlsx") %>% 
  read.xlsx()

matching_file <- file.path("input", "match_files","match_file.xlsx") %>% 
  read.xlsx()
#temp <- read.csv(input_file_path)

input_data <- input_file_path %>%
  read.csv() %>%
  select(
    Product_Name,
    Consumer_ID,
    Consumer_Attribute,
    Consumer_Response,
    Consumer_Questionnaire_Type,
    Consumer_Question_Tag
  ) %>% 
  mutate(Consumer_Response = str_trim(str_remove_all(Consumer_Response, "=.*$"))) 

# create attribute key

unique_atts <- trimws(input_data$Consumer_Attribute, which = "left") %>%
  unique()

att_key <- unique_atts %>%
  str_split(pattern = ";") %>%
  map( ~ .[[length(.)]]) %>%
  as.character() %>%
  set_names(unique_atts) %>%
  enframe(name = "Consumer_Attribute", value = "Attribute")

# create response

unique_resps <- input_data$Consumer_Response %>%
  unique()

resp_key <- unique_resps %>%
  str_split(pattern = ";") %>%
  set_names(unique_resps) %>%
  enframe(name = "Consumer_Response", value = "Responses")

# put data together

prepped_data <- input_data %>%
  left_join(att_key) %>%
  left_join(resp_key) %>%
  select(
    Type = Consumer_Questionnaire_Type,
    Tag = Consumer_Question_Tag,
    ID = Consumer_ID,
    Product = Product_Name,
    Attribute,
    Responses
  )
  #mutate(Responses = unlist(map(str_split(Responses, pattern = "="),function(l) l[[1]]))) 
  

prepped_data_screener <- prepped_data %>% 
  filter(Type == 'Screener')

prepped_data_usage <- prepped_data %>% 
  filter(Type == 'Blind Product Usage') 



# # separate into screener and product specific data
# 
# split_data <- prepped_data %>%
#   split(.$Type)
# 
# screener_data <- split_data$Screener %>%
#   select(ID, Attribute, Responses) %>%
#   unique()


#drop_str_vect <- c("Which of the following brands of multivitamin supplements do you currently use ?")

st <- " Which of the following brands of multivitamin supplements do you currently use?"
#st <- " Which of the following brands of multivitamin supplements do you currently use?"
#drop_pattern <- str_c("(", str_c(drop_str_vect, collapse = "|"), ")")


prepped_data_screener <- prepped_data_screener %>% 
  filter(Tag != "Open_End") %>% 
  #mutate(Attribute = str_remove_all(Attribute, st)) %>% 
  unnest(cols = c(Responses)) 



prepped_data_usage <- prepped_data_usage %>% 
  filter(Tag != "Open_End") %>% 
  #mutate(Attribute = str_remove_all(Attribute, st)) %>% 
  unnest(cols = c(Responses)) %>% 
  set_names(str_trim(colnames(.)))


    
# prepped_data <- prepped_data %>% 
#   filter(Tag != "Open_End") %>% 
#   mutate(Attribute = str_remove_all(Attribute, drop_str_vect[[1]])) %>% 
#   mutate(Attribute = str_remove_all(Attribute, drop_str_vect[[2]])) %>% 
#   unnest(cols = c(Responses))

prop_thresh <- 0.015


comment_suffix <- "as much detail as possible."

prepped_wide_usage <- prepped_data_usage %>% 
  select(-c(Type, Tag)) %>%
  pivot_wider(names_from = Attribute, values_from = Responses) %>% 
  
   select(-ends_with(comment_suffix)) %>% 
   select(-contains("multivitamin supplements")) %>% 
   set_names(trimws(str_remove(colnames(.), "Â"),which="left"))

#column_matching$Legend

prepped_wide_screener <- prepped_data_screener %>% 
  select(-c(Type, Tag)) %>%
  pivot_wider(names_from = Attribute, values_from = Responses)

append_usage <- prepped_wide_screener %>% 
  select(ID, `Exact Age`)

prepped_wide_usage <- prepped_wide_usage %>% 
  select(all_of(column_matching$Legend)) %>%
  set_names(column_matching$Variable.name)


cons_data <- prepped_wide_usage %>%
  left_join(matching_file, by = c("Product" = "Consumer")) %>%
  select(-Product,-Products,-Sensory) %>%
  select(ID,Product_name, everything()) %>%
  left_join(append_usage, by = c("ID")) %>%
  select(-Age_group, -Product_placed, -First_product)


# cons_data <- cons_data %>% 
#   mutate(Overall_opinion = as.double(Overall_opinion)) %>% 
#   group_by(Product_name) %>% 
#   summarize_at(vars(Overall_opinion), mean) %>% 
#   clipr::write_clip()

#Put filter for exact age: 36-66 
cons_data <- prepped_wide_usage %>%
  left_join(matching_file, by = c("Product" = "Consumer")) %>%
  select(-Product,-Products,-Sensory) %>%
  select(ID,Product_name, everything()) %>%
  left_join(append_usage, by = c("ID")) %>%
  select(-Age_group, -Product_placed, -First_product) %>%
  filter(`Exact Age` >= 36 & `Exact Age` <= 66) %>%
  select(-`Exact Age`)

#Age 18-35
# cons_data <- prepped_wide_usage %>%
#   left_join(matching_file, by = c("Product" = "Consumer")) %>%
#   select(-Product,-Products,-Sensory) %>%
#   select(ID,Product_name, everything()) %>%
#   left_join(append_usage, by = c("ID")) %>%
#   select(-Age_group, -Product_placed, -First_product) %>%
#   filter(`Exact Age` >= 18 & `Exact Age` <= 35) %>%
#   select(-`Exact Age`)
# 
# 

cons_data <- cons_data %>%
  mutate_all(as.character) %>% 
  filter(Product_name != "Garden of life_women") %>% 
  mutate(Overall_opinion = as.double(Overall_opinion))

test_data <- cons_data %>%
  filter(Product_name == "Centrum_women") %>% 
  mutate(Liking_swallow = as.double(Liking_swallow))
# 
# 
# cons_data %>% 
#   group_by(Product_name) %>% 
#   summarize_at(vars(Overall_opinion), mean) %>% 
#   clipr::write_clip()

# colnames(cons_data) %>% 
#   str_replace_all("_", " ") %>% 
#   str_to_title() %>% 
#   clipr::write_clip() 
  
source(file.path("scripts","data_cleaning","Tablet","sensory_cleaning_2.R"))


sensory_list <- list(cons = cons_data, sensory = sens_data) %>% 
  enframe(name = "Source", value = "Data")

data_path <- file.path(
  "input",
  "imported_data",
  "tablet_imported_data.rds"
)

saveRDS(sensory_list, data_path)
readRDS(data_path)

# tablet_data <- cons_data %>% 
#   inner_join(sens_data, by = c("Product_name"))
# 
# 
# temp <- as.data.frame(sapply(prepped_wide_usage, as.numeric))


# temp <- prepped_wide %>% 
#   select_if(is.numeric)
# 
# output_file_path <- file.path("input","raw_data" "tablet_consumer.xlsx")
# 
# write.xlsx(prepped_wide, file = "tablet_consumer.xlsx")


