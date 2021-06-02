
library(tidyverse)
library(lmtest)
#1) Simulate a 100-observation autocorrelated timeseries with `arima.sim`, with a first order autoregressive coefficient of 0.5. Also make a time vector of `1:100`


x1 <- arima.sim(list(order = c(1,0,0), ar = 0.5), n = 100)

#2) Plot the data. 

plot(x1)

#3) Regress the timeseries against time with an OLS model. Does the model appear to be statistically significant?

lm_1<- lm(x1~time(x1))

anova(lm_1) 
#4) Plot the model diagnostics, including an `acf` and `pacf` of the residuals.
par(mfrow=c(3))
plot(lm_1)
acf(resid(lm_1))

pacf(resid(lm_1))

#5) Use the Durbin-Watson test to test the residuals for autocorrelation.

dwtest(lm_1)

#6) Fit a gls with an appropriate correlation structure. Is this a better model? How have the p-value and effect size changed?
x1_gls <- gls(x1~time(x1), corr=corAR1())
summary(x1_gls)
##not sure if the p value got any better. seems like it got worse 
#7) Repeat the above 1000 times and find how autocorrelation affects the distribution of the p-value and the effect size.
library(broom.mixed)

rerun(1000, {x1.sim <- arima.sim(list(order = c(1,0,0), ar = 0.5), n = 100 )

lm(x1.sim~time(x1.sim))}) %>% 
  map_dfr(glance) %>% 
  ggplot(aes(x = p.value)) +
  geom_histogram()
## Real data#####

#1) The built-in dataset LakeHuron has annual lake level data from 1885 to 1972
#Load the data with the command data(LakeHuron)

data(LakeHuron)
table(LakeHuron)

LakeHuron_dt <- LakeHuron

plot(LakeHuron_dt)

#2) Plot the data.
plot(LakeHuron_dt)

#3) Regress the LakeHuron lake level against year using a linear model. Is there a significant trend?
lm_df <- lm(LakeHuron~time(LakeHuron))
summary(lm_df) 
# p value < 0.05, seems legit. 

#4) Plot the autocorrelation and partial autocorrelation functions for the residuals from this regression. Interpret them.
acf(resid(lm_df))
#5) Fit an autoregressive models to the residuals. Compare the results with your interpretation of the PACF plot.
pacf(resid(lm_df))

#6) Fit a gls model using a corAR1 correlation structure. Test if the correlation structure is necessary. Is the trend significant? 
year <- time(LakeHuron_dt)
fit_gls <- gls(LakeHuron_dt~year)
summary(fit_gls)

fit2_gls<-gls(LakeHuron_dt~year, corr=corAR1())
summary(fit2_gls)

anova(fit_gls,fit2_gls) #looks like the structure is nessesary? W ith the p-value.

#7) Fit a gls model using a corARMA correlation structure with two AR terms. Is this model an improvement?


fit3_gls<-gls(LakeHuron_dt~year, corr=corARMA(p=1, q=1))
summary(fit3_gls)

anova(fit_gls,fit2_gls,fit3_gls)
  