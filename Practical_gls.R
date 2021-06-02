library(datasets)
library(tidyverse)
library(nlme)
data(iris)
summary(iris)

iris_dt <- iris


#### plotting the data species- Petal length ####
iris_dt %>%
  ggplot(aes(x = Species, y = Petal.Length, )) +
  geom_boxplot( colour = c("dark blue","red","pink")) +
  labs(title = "Petal length in different species", x = "Species", y = "Petal lenght(cm)")



### Variance petal length ea species ####
iris_dt %>% 
  group_by(Species)%>% 
  summarise(var_species = var(Sepal.Length, na.rm =TRUE))

virginica_var <- iris_dt%>%
  filter(Species == "virginica")
var(virginica_var$Sepal.Length)
##Fit an anova using `lm` between `Petal.Length` and species and examine the diagnostic plots.##
fit_lm1 <- lm(Petal.Length~Species, data = iris_dt)
summary(fit_lm1)

x11()
par(mfrow=c(2,2))
plot(fit_lm1)


##Fit a `gls` for the same model. Have the coefficients changed? (remember to load ´nlme´ package)##



fit_gls1 <- gls(Petal.Length~Species, data = iris_dt)
summary(fit_gls1)

##Fit a `gls` for the same model but allow the variance to be different for each species by adding `varIdent` to the `weights` argument of the model.##
fit_gls2 <- gls(Petal.Length~Species, data = iris_dt, weights = varIdent(form = ~ +1|Species))
summary(fit_gls2)

##Use `AIC` to test if this is a better model.##

anova(fit_gls1, fit_gls2)
