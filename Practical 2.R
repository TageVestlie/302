library(tidyverse)
library(palmerpenguins)


#1. Import the penguin data from the `palmerpenguin` package
new_penguins<- penguins%>% 
  drop_na()
View(new_penguins)

#Do big birds have big beaks? Fit a linear model between bill length and body mass, and interpret the coefficients.

new_penguins %>% 
  ggplot(aes(x=body_mass_g,y = bill_length_mm))+
  geom_point() +
  stat_smooth(method = "lm" , col = "red") +
  labs(x= "Body mass(g)", y= "bill length(mm)" )

fit_lm1 <- lm(bill_length_mm~body_mass_g, data = new_penguins)
summary(fit_lm1)


#3. Examine the diagnostics plots. Is everything OK?#
x11()
par(mfrow=c(2,2))
plot(fit_lm1)


#3. Calculate the residual sum of squares 
#(find sum the squared difference between the estimates calculated from the coefficients and the observed values).#
deviance(fit_lm1)
sum(resid(fit_lm1)^2)

#Add species to the model as a predictor
new_penguins %>% 
  ggplot(aes(x=body_mass_g,y = bill_length_mm, fill=species))+
  geom_point() +
  stat_smooth(method = "lm" , col = "red") +
  labs(x= "Body mass(g)", y= "bill length(mm)" )

fit_lm2 <- lm(bill_length_mm~body_mass_g*species, data = new_penguins)
summary(fit_lm2)