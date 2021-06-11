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
  mutate(Consumer_Response = unlist(map(str_split(Consumer_Response, pattern = "="),function(l) l[[1]])))

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
  unnest(cols = c(Responses))
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


prepped_wide_usage <- prepped_wide_usage %>% 
  select(all_of(column_matching$Legend)) %>%
  set_names(column_matching$Variable.name)



cons_data <- prepped_wide_usage %>% 
  left_join(matching_file, by = c("Product" = "Consumer")) %>% 
  select(-Product,-Products,-Sensory) %>% 
  select(ID,Product_name, everything())

saveRDS(cons_data,file = "output/consumer_data_tablet.rds")

prepped_wide_screener <- prepped_data_screener %>% 
  select(-c(Type, Tag)) %>%
  pivot_wider(names_from = Attribute, values_from = Responses)

source(file.path("scripts","data_cleaning","Tablet","sensory_cleaning.R"))

tablet_data <- cons_data %>% 
  inner_join(sens_data, by = c("Product_name"))


temp <- as.data.frame(sapply(prepped_wide_usage, as.numeric))


# temp <- prepped_wide %>% 
#   select_if(is.numeric)
# 
# output_file_path <- file.path("input","raw_data" "tablet_consumer.xlsx")
# 
# write.xlsx(prepped_wide, file = "tablet_consumer.xlsx")


