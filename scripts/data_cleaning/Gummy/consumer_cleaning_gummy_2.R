library(tidyverse)
library(openxlsx)
library(janitor)
library(readxl)

file_name <- "gummy_consumer.csv"

input_file_path <- file.path("input", "raw_data", "RH03561_Centrum Category Appraisal (Gummy)_Conusmer", file_name)


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



# prepped_data <- prepped_data %>%
#   filter(Tag != "Open_End") %>% 
#   mutate(Attribute = str_remove_all(Attribute, drop_str_vect[[1]])) %>% 
#   mutate(Attribute = str_remove_all(Attribute, drop_str_vect[[2]])) %>% 
#   unnest(cols = c(Responses))

prop_thresh <- 0.015


comment_suffix <- "as much detail as possible"

#To do: Change names of the columns in prepped_data_usage

prepped_wide_usage <- prepped_data_usage %>% 
  select(c(-Type, -Tag)) %>%
  pivot_wider(names_from =Attribute, values_from = Responses)
  
prepped_wide_usage_generic <- prepped_data_usage %>% 
  select(-c(Type, Tag)) %>%
  pivot_wider(names_from = Attribute, values_from = Responses) %>% 
  select(-contains("detail")) %>%
  select(-contains("Check all that apply")) %>% 
  select(-starts_with("Day")) %>% 
  select(-contains("multivitamin supplements")) %>% 
  pivot_longer(cols = !ID & !Product, names_to = "Name", values_to = "Value") %>% 
  mutate(Value = unlist(map(str_split(Value, pattern = "="),function(l) l[[1]]))) %>% 
  pivot_wider(names_from = "Name", values_from = "Value") %>% 
  set_names(trimws(str_remove(colnames(.), "Â"),which="both"))

prepped_wide_usage_generic[ prepped_wide_usage_generic == "NULL"] <- NA
prepped_wide_usage_generic = prepped_wide_usage_generic[ complete.cases(prepped_wide_usage_generic),]


column_matching <- file.path("input", "match_files", "gummy_column_name.xlsx") %>% 
  read.xlsx()

prepped_wide_usage_generic <- prepped_wide_usage_generic %>% 
  select(all_of(column_matching$Legend)) %>%
  set_names(column_matching$Variable.name) %>%
  #select(!Product) %>%   
  mutate(Overall_Opinion = as.integer(Overall_Opinion))
  #mutate_if(Overall_Opinion = as.integer(Overall_Opinion))

prepped_wide_usage_color <- prepped_data_usage %>% 
  select(-c(Type, Tag)) %>%
  pivot_wider(names_from = Attribute, values_from = Responses) %>% 
  select(-ends_with(comment_suffix)) %>%
  select(-contains("multivitamin supplements")) %>% 
  select(ID, Product,starts_with("Day")) %>% 
  select(-ends_with("- Product")) %>% 
  select(-ends_with("Order")) %>% 
  pivot_longer(cols = !ID & !Product,names_to = "Name", values_to = "Value") %>% 
  mutate(Value = unlist(map(str_split(Value, pattern = "="),function(l) l[[1]]))) %>% 
  mutate(Day = unlist(map(str_split(Name, pattern = "-"), function(l) str_trim(l[1])))) %>% 
  mutate(Evaluation_number = unlist(map(str_split(Name, pattern = "-"), function(l) str_trim(l[2])))) %>% 
  mutate(Question = unlist(map(str_split(Name, pattern = "-"), function(l) str_trim(l[3])))) %>% 
  select(ID, Product, Day, Evaluation_number, Question, Value) %>% 
  filter(Value != "No Response") %>% 
  pivot_wider(names_from = "Question", values_from = "Value") %>% 
  drop_na()


# unique_color <- prepped_wide_usage_color %>% 
#   select(contains("color")) %>% 
#   pivot_longer(cols = everything(), names_to = "names", values_to = "values")
# 
# 
# unlist(unique(unique_color$values))
#Split the columns to make responses clearer

#

prepped_wide_screener <- prepped_data_screener %>% 
    select(-c(Type, Tag)) %>%
    pivot_wider(names_from = Attribute, values_from = Responses, names_repair = "minimal")
# temp <- prepped_wide %>% 
#   select_if(is.numeric)
# 
# output_file_path <- file.path("input","raw_data" "tablet_consumer.xlsx")
# 
# write.xlsx(prepped_wide, file = "tablet_consumer.xlsx")


age <- prepped_wide_screener %>%
  select(ID, `SAMPLE AGE`)

cons_data <- prepped_wide_usage_generic %>%
  left_join(age, by = c("ID" = "ID"), suffix = c("_generic","age")) %>%
  mutate(`SAMPLE AGE` = unlist(`SAMPLE AGE`)) %>% 
  mutate_all(as.character) %>% 
  mutate(Overall_Opinion = as.integer(Overall_Opinion)) %>% 
  mutate_at(vars(-ID,-Product),funs(as.double))


source(file.path("scripts","data_cleaning","Gummy","relevance_product_based.R"))


sensory_list <- list(cons = cons_data, sensory = sens_data) %>% 
  enframe(name = "Source", value = "Data")


#gummy_imported_data has all the attributes: Analytical+sensory+consumer+bases
data_path <- file.path(
  "input",
  "imported_data",
  "gummy_imported_data.rds"
)

saveRDS(sensory_list, data_path)
readRDS(data_path)
