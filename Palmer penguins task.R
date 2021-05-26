library(tidyverse)
library(palmerpenguins)
View(penguins)

new_penguins<- penguins%>% 
  drop_na()

new_penguins %>% 
  summarise(mean = mean(bill_length_mm), 
            medial= median(bill_length_mm),
            range= range(bill_length_mm),
            variance=var(bill_length_mm))

new_penguins %>% 
  summarise(sd(bill_length_mm))

bill_length_mean <- new_penguins %>% 
  summarise(mean = mean(bill_length_mm))

bill_length_median <- new_penguins %>% 
  summarise(median = median(bill_length_mm))

new_penguins %>% 
  ggplot(aes(x=bill_length_mm))+
  geom_histogram(binwidth=1, color = "black", fill = "magenta", alpha=0.3) +
  labs(x="Bill length(mm)",y = "Count") +
  geom_vline(xintercept = bill_length_mean$mean, linetype = 2,colour = "maroon", size= 1) +
  geom_vline(xintercept = bill_length_median$median, linetype = 3,colour = "dark blue", size = 1)