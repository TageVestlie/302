library(tidyverse)
library(readxl)
library(lubridate)
library(scales)


#### Air temp part #####
# - Calculate mean summer air temperatures each site. Use the logger "gradient". The OTC logger is part of another experiment. 

airtemp_file <- ("China_2013_2016_AirTemp_month.csv")
airtemp <- read.csv(airtemp_file)

airtemp <- airtemp %>% 
  rename (date = month) %>% 
  rename (temperature = value)
  


airtemp_mean <- airtemp %>% 
  filter(logger == "gradient") %>% 
  separate(col = date, into = c("year","date","day" ))%>% 
  filter(date %in% c("06","07","08"))%>%
  group_by(site) %>% 
  summarise(summer_temp = mean(temperature, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(site)

view(airtemp_mean)

#### Biomass part ####
#### Merging each plot on each sites production to a total production. 
biomass_file <- excel_sheets("Biomass2015.xls") %>% 
  map_dfr(~ read_excel("Biomass2015.xls", sheet = .x))


mergedata_df <- 
  biomass_file %>% 
  group_by(plot,site) %>% 
  summarise(total_production = sum (production, na.rm = T)) %>% 
  ungroup()

View(mergedata_df)
 #merging the full sett of different slides in excel to one dataset. 

#### The joining of the biomass and the temperature####
biomass_temperature <- mergedata_df %>%
  left_join(airtemp_mean, by = "site") %>% 
  mutate(summer_temp_sq = summer_temp^2)


#creating a model 
df_lm <- lm(total_production ~ summer_temp + summer_temp_sq, biomass_temperature) ## have to write about why i added the summer_temp_sq

summary(df_lm)


# creating diagnostik plots 
x11()
par(mfrow=c(2,2))
plot(df_lm)


#creating a graphical model


biomass_temperature %>% 
  ggplot(aes(x = summer_temp, y = total_production)) +
  geom_point(color = "skyblue") +
  geom_smooth(method = lm, 
              formula = y~ poly(x, 2), #adding the polynomial because of the "fall# of biomass at higher temperatures, straight line did not fit as well 
              color = "orange") +
  #cleaner theme 
  theme_minimal() +
  #chaning the labels 
  labs(title = "Biomass production and summer temperatures",
       subtitle = "Polynomial fit between biomass production and 
       mean summer temperatures by plot",
       x = "Mean summer temperature",
       y = "Biomass") +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +# adding more numbers to the y-axis
  scale_x_continuous(breaks = pretty_breaks(n = 10)) # adding more numbers to the x-axis


