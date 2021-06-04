## hypothesis testing
library(tidyverse)
library(palmerpenguins)
library(MASS)
library(AICcmodavg)

#1. Import the palmerpenguin data
penguins_df <- penguins %>% 
  drop_na()

#2. Test the hypothesis that bill length differs between species.
penguins_df %>% 
  ggplot(aes(x = species, y = bill_length_mm, fill = sex)) +
  geom_boxplot()



lm_df <- lm(bill_length_mm~species, data = penguins_df)
anova(lm_df)

#3. Test the hypothesis that bill length differs by sex in addition to species.

lm_df2 <- lm(bill_length_mm~species + sex, data = penguins_df)

anova(lm_df,lm_df2)
#4. How should the p-values be interpreted.

#both p-values are both unde 0.05, seems to be sigificant.

## Exploratory model building 

#Normally you would be doing this on a separate data set


#5. Use forward selection to find the best model to explain bill length.
mod3 <- lm(bill_length_mm~.^2-year, data = penguins_df)
summary(mod)
mod2 <- stepAIC(mod, k = 2)

#6. Build a set of candidate models to explain bill depth using one or two predictors.  
mods<- list()
mods$m0 <- lm(bill_length_mm~ 1, data = penguins_df)
mods$m1 <- lm(bill_length_mm~ species , data = penguins_df)
mods$m2 <- lm(bill_length_mm~ sex , data = penguins_df)
mods$m3 <- lm(bill_length_mm~ sex + species , data = penguins_df)
mods$m4 <- lm(bill_length_mm~ sex * species , data = penguins_df)


#9. Extract the AIC from each  models (hint use function `AIC`). Which is the better model?
sapply(mods, AIC)
#10. Calculate the deltaAIC for each model.
aictab(mods)
#11. Calculate the AIC weights for each model. Interpret these weights.

# weights for model 1 is 0.56
# weights for model 2 is 0.44


## Collinearity 

#12. Make a model predicting bill_length from all other variables. Find the VIF of each predictor. Are there any problem variables? `olsrr::ols_vif_tol`
mod3 <- lm(bill_length_mm~.^2-year, data = penguins_df)
library(olsrr)

olsrr::ols_vif_tol(mod3)
#13. Use `GGally::ggpairs()` to plot the data to try to identify the cause of any high vif.

library(GGally)
GGally::ggpairs(mod3)

#14. Use `MASS::mvrnorm()` to simulate 100 observation of two predictor variables (x and z) with a given correlation. Simulate a response variable y = b0 + b1x + b2z. Test how the uncertainty in the coefficients changes with the correlation (and hence vif) of the predictor variables.