# This is a small exercise for fitting VAR models in R
# Using USconsumption data

library(fpp2)
library(tidyverse)
library(vars)

autoplot(usconsumption)

ar(usconsumption, order = 2)

#This will gives the optimal VAR order according to different information criteria
VARselect(usconsumption, lag.max = 8, type = "const")

#Fitting VAR model with order 3
var <- VAR(usconsumption, p=3, type = "const")

#tests the serial correlation of residuals
serial.test(var, lags.pt = 10, type = "PT.asymptotic")

#Forecasting
Fc <- forecast(var)
