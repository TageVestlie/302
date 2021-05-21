setwd("C:/Users/tagev/OneDrive/Documents/302")
library(tidyverse)
library(stringr)
library(readxl)
library(dplyr)
data_L <- read_excel("biomass2015.xls", sheet = 1)
data_M <- read_excel("biomass2015.xls", sheet = 2)
data_A <- read_excel("biomass2015.xls", sheet = 3)
data_H <- read_excel("biomass2015.xls", sheet = 4)

mergedata_L_df <- 
  data_L %>% 
  group_by(plot,site) %>% 
  summarise(total_production = sum(production, na.rm = T)) %>% 
  ungroup()
  


mergedata_M_df <- 
  data_M %>% 
  group_by(plot,site) %>% 
  summarise(total_production = sum(production, na.rm = T)) %>% 
  ungroup()

mergedata_A_df <- 
  data_A %>% 
  group_by(plot,site) %>% 
  summarise(total_production = sum(production, na.rm = T)) %>% 
  ungroup()

mergedata_H_df <- 
  data_H %>% 
  group_by(plot,site) %>% 
  summarise(total_production = sum(production, na.rm = T)) %>% 
  ungroup()

tosett <- dplyr::full_join(mergedata_H_df, mergedata_A_df)

tredjesett <- dplyr::full_join(tosett, mergedata_M_df)

fullsett <- dplyr::full_join(tredjesett, mergedata_L_df)

plot1 <- ggplot(data = fullsett, aes(site, total_production))+
  geom_boxplot(fill = "orange", alpha = 0.3)+
  labs( x = "Site", y= "Biomass") + 
  theme_light()
  
plot1

