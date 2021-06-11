library(tidyverse)
library(stringr)
library(openxlsx)
library(reshape2)
library(corrplot)
library(dendextend)
#
file_name <- 'RH03575_CentrumGummy_Sensory.csv'
source(file.path("scripts", "supporting", "load_constants_gsk.R"))

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

texture_attributes <- c("First_Bite_-_Resistance","Springyness","Roughness","Stickiness","Cohesiveness","Slipperyness","Dissolve_Rate","Astringent_")


texture_data <- sens_data %>%
  select(Sample_Name, any_of(texture_attributes)) %>% 
  drop_na() %>% 
  group_by(Sample_Name) %>% 
  summarize_at(vars(texture_attributes), mean) %>% 
  ungroup()
  

source(file.path("scripts","data_cleaning","Gummy","analytical_cleaning_gummy.R"))

cluster_data <- texture_data %>% 
  left_join(analytical_data, by = c("Sample_Name" = "Product_Name")) %>% 
  group_by(Sample_Name) %>% 
  summarize_all(mean) %>% 
  ungroup() %>% 
  pivot_longer(cols = !Sample_Name, names_to="Variable", values_to = "Value") %>% 
  select(-Sample_Name)


row.names(cluster_data) <- cluster_data$Variable
  
analytical_data <- analytical_data %>% 
  select(-Product_Name)
texture_data <- texture_data %>% 
  select(-Sample_Name)

corr_mat <- cor(texture_data,analytical_data)


melted_cormat <- corr_mat %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "texture_attributes") %>% 
  pivot_longer(-texture_attributes, names_to = "analytical_attributes")

heat_map <- ggplot(melted_cormat, aes(x = texture_attributes, y = analytical_attributes, fill = value)) +
  geom_tile() +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  scale_fill_gradient2(
    low = gsk_pink,
    high = gsk_teal,
    name = "Correlation",
    breaks = c(-1, 0, 1),
    labels = c("-1", "0", "1"),
    trans = 'reverse',
    midpoint = 0
  ) + 
  theme_minimal() + 
  theme(
    axis.title = element_blank()
  )

heat_map

# order samples
res_hclust <- cluster_data %>%
  dist() %>%
  hclust(method = "ward.D2")
order_hclust <- res_hclust %>%
  order.hclust()
ordered_samples <- rownames(cluster_data)[order_hclust]


# order attributes
res_hclust <- melted_cormat %>%
  t() %>%
  dist() %>%
  hclust(method = "ward.D2")


order_hclust <- res_hclust %>%
  order.hclust()

ordered_attribs <-
  rownames(melted_cormat %>% t())[order_hclust]

#drop_na() 
# summarize_all(mean) %>% 
# left_join(matching_file, by = c("Sample_Name" = "Sensory")) %>% 
# select(-Consumer) %>%
# select(Sensory, everything())


