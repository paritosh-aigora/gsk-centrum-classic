library(tidyverse)
library(ggplot2)
library(openxlsx)
library(purrr)
library(ggrepel)

comparison_data <- file.path("input", "match_files", "centrum women comparison.xlsx") %>% 
  read.xlsx() %>% 
  rename(`Organic:Centrum Women 2` = `Centrum.Women.Multivitamins.-.Product.C-.Red/Purple.Gummy`,
          `Category: Centrum Women` = `Centrum.Multivitamin.Women.Assorted.Natural.Fruits`)


comparison_plot <- ggplot(data = comparison_data,aes(x=`Category: Centrum Women`, y =`Organic:Centrum Women 2`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  #geom_text(aes(label = Sample_Name), size = 3) + 
  geom_text_repel(aes(label = Sample_Name))

officer::read_pptx() %>%
  officer::add_slide() %>%
  officer::ph_with(rvg::dml(ggobj = comparison_plot),
                   officer::ph_location(width = 9, height = 5)) %>%
  print(target = "comparison_centrum_women.pptx")
