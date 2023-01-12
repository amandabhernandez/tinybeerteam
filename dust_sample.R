### Author: AHz
### Date: 10/11/2022
### Written in: R version 4.2.1
### Purpose: Load dust data


# set working directory to file location
source_file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(source_file_loc)

library(pacman)
p_load(tidyverse)
p_load(janitor)

keep_metals <- c("As", "Ca", "Cd", "Co", "Cr", "Fe", "K", "Mn", "Mo", "Ni",
                 "Pb", "Rb", "Sd", "Sc", "Se", "Sr", "Ti", "Zn", "Zr")

mydust <- readxl::read_xlsx("data/tbt_dustsamples.xlsx", col_types = c("text")) %>% 
  clean_names() %>% 
  select(-c(sequence:flags)) %>% 
  filter(reading_no != 504)


mydust_long <- mydust %>% 
  pivot_longer(names_to = "metal", values_to = "conc", cols = mo:pd_error) %>% 
  separate(metal, sep = "_", into = c("metal", "error")) %>% 
  mutate(error = case_when(is.na(error) ~ "conc", 
                           TRUE ~ "error"))  %>% 
  pivot_wider(names_from = "error", values_from = "conc") %>% 
  mutate(LOD_flag = case_when(conc == "<LOD" ~ "<LOD", 
                              TRUE ~ conc),
         conc = as.numeric(conc),
         error = as.numeric(error),
         metal = snakecase::to_title_case(metal)) %>% 
  filter(metal %in% keep_metals)


ggplot(mydust_long , aes(x = reading_no, y = conc)) + 
  geom_point() + 
  geom_errorbar(mydust_long, mapping=aes(x=reading_no, 
                                         ymin=conc-error, ymax=conc+error), 
                width=0.2, size=0.2) + 
  facet_wrap(~metal, scales = "free_y") + 
  theme_bw()

ggplot(mydust_long, aes(x = metal, y = conc)) + 
  geom_boxplot()
