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


tobbe <- merge(mergedata_A_df, mergedata_H_df, by = "site")

