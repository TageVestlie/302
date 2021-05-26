setwd("C:/Users/tagev/OneDrive/Documents/302")
library(tidyverse)
library(readxl)

excel_sheets("C:/Users/tagev/OneDrive/Documents/302/biomass2015.xls")

datafile <- "biomass2015.xls"

####Import data ####
data_L <- read_excel(datafile, sheet = "Site L")
data_M <- read_excel(datafile, sheet = "Site M")
data_A <- read_excel(datafile, sheet = "Site A")
data_H <- read_excel(datafile, sheet = "Site H")

##### Grouping the plots ####
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

#### Merging the datasets ####
tosett <- dplyr::full_join(mergedata_H_df, mergedata_A_df)

tredjesett <- dplyr::full_join(tosett, mergedata_M_df)

fullsett <- dplyr::full_join(tredjesett, mergedata_L_df)
#kan bruke bind_rows(Site_L, Site_H, ...)

#### Plotting ####

p1 <-fullsett %>%
  ggplot(aes(site, total_production))+
  geom_boxplot(fill = "orange","blue","green","red", alpha = 0.3)+
  labs( x = "Site", y= "Biomass") + 
  theme_light()
  
p1


