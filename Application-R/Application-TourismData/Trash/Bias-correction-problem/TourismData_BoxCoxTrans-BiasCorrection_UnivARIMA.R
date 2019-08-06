
# Univariate ARIMA models were fitted for the transformed data (Used BoxCox transformation)
# The back transformed data will be adjusted for bias to get unbiased forecasts


library(tidyverse)
library(fpp2)
library(hts)

OvernightTrips_Region <- read_csv("OvernightTrips_2017.csv")[,-(1:3)]

# generating the hierarchy

Hierarchy <- suppressMessages(hts(OvernightTrips_Region, list(7, c(6,5,4,4,3,3,2), 
                                                            c(2,2,1,4,4,1,3,1,3,6,7,3,4,3,2,3,
                                                              3,4,2,3,1,1,1,2,2,3,4))))
AllTS <- allts(Hierarchy) %>% 
  as_tibble()

## Replacing the odd value in `Adelaide Hills` with the average of its consecutive numbers ##

AllTS %>% 
  select(`Adelaide Hills`) %>% 
  filter(`Adelaide Hills` > 80) %>% 
  as.numeric() -> a

which(AllTS$`Adelaide Hills`==a) -> x

AllTS %>% select(`Adelaide Hills`) %>% 
  filter(between(row_number(), 59, 61)) %>% 
  filter(`Adelaide Hills` < 80) %>% 
  summarise(mean(`Adelaide Hills`)) %>% 
  as.numeric() -> b

AllTS_new <- AllTS %>% 
  mutate(`Adelaide Hills` = case_when(`Adelaide Hills`%% a == 0 ~ b, 
                                      TRUE ~ as.double(`Adelaide Hills`)))

n <- ncol(AllTS)
m <- ncol(OvernightTrips_Region)
H <- 6
L <- 100 #Size of the training sample 
C <- nrow(AllTS) - L # Number of replications


# Generating the summing matrix

S <- smatrix(Hierarchy)


# Data frame to store results

DF <- tibble("Series" = character(),
             "F-method" = character(),
             "R-method" = character(),
             "Forecast_Horizon" = double(),
             "Forecasts" = double(),
             "Actual" = double(),
             "Replication" = integer())


Start <- Sys.time()

for (j in 1:139) {#C = 140
  

  Train <- AllTS_new[j:(L+j-1),]
  Test <- AllTS_new[(L+j):nrow(AllTS_new),]
  Names <- colnames(AllTS_new)
  
  Base_forecasts_biased <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  Base_forecasts_unbiased <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  
  Residuals_BackTransformed <- matrix(NA, nrow = nrow(Train), ncol = n)
  Residuals_BackTransformed_biasadjust <- matrix(NA, nrow = nrow(Train), ncol = n)
 
  
  Start_fit <- Sys.time()
  
  for (i in 1:n) {
    
    TS <- Train[,i] %>% as.ts()

    
    fit_bias <- auto.arima(TS, lambda = "auto", biasadj = FALSE)
    fit_unbiased <- auto.arima(TS, lambda = "auto", biasadj = TRUE)
    
    Base_forecasts_biased[,i] <- forecast(fit_bias, h=min(H, nrow(Test)), 
                                          biasadj = FALSE)$mean
    Base_forecasts_unbiased[,i] <- forecast(fit_unbiased, h=min(H, nrow(Test)), 
                                            biasadj = TRUE)$mean

    Residuals_BackTransformed[,i] <- residuals(fit_bias, h=1, type = "response")
    Residuals_BackTransformed_biasadjust[,i] <- residuals(fit_unbiased, h=1, 
                                                          type = "response")
    
    

    
  }
  
  End_fit <- Sys.time()
  

  #--Adding base forecasts to the DF--#
  
  if(j < C){
    Test_1 <- Test[1:min(H, nrow(Test)),]%>% as.matrix()
  } else {
    Test_1 <- Test
  }
  
  DF <- DF %>% add_row(Series = rep(Names, each = min(H, nrow(Test_1))), 
                       `F-method` = rep("ARIMA_bias", n*min(H, nrow(Test_1))), 
                       `R-method` = "Base", 
                       Forecast_Horizon = rep(1:min(H, nrow(Test_1)), n),
                       Forecasts = as.numeric(Base_forecasts_biased),
                       Actual = as.numeric(Test_1), 
                       Replication = rep(j, n*min(H, nrow(Test_1)))) 
  
  DF <- DF %>% add_row(Series = rep(Names, each = min(H, nrow(Test_1))), 
                       `F-method` = rep("ARIMA_unbias", n*min(H, nrow(Test_1))), 
                       `R-method` = "Base", 
                       Forecast_Horizon = rep(1:min(H, nrow(Test_1)), n),
                       Forecasts = as.numeric(Base_forecasts_unbiased),
                       Actual = as.numeric(Test_1), 
                       Replication = rep(j, n*min(H, nrow(Test_1)))) 
  
  
 
  
}

End <- Sys.time()


DF %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF

DF %>% 
  group_by(`F-method`, `R-method`, Forecast_Horizon) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  spread(key = Forecast_Horizon, value = MSE)

DF %>% 
  group_by(`F-method`, `R-method`, Forecast_Horizon, Replication) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  spread(key = `F-method`, value = MSE) %>% 
  mutate("Bias-Unbias" = `ARIMA_bias` - `ARIMA_unbias`) %>% 
  ggplot(aes(x = Replication, y = `Bias-Unbias`)) + geom_point() + 
  facet_wrap(~Forecast_Horizon)


#######
DF %>% 
  filter(`F-method` == "ARIMA_unbias", 
         `R-method` == "Base", 
         SquaredE > 1e6) %>%
  distinct(Replication) %>%
  as.matrix() %>% 
  as.numeric() -> J
  
  
DF %>% 
  filter(`F-method` == "ARIMA_unbias", 
         Replication == J[120], 
         `R-method` == "Base", 
         SquaredE > 1e6) %>% 
  distinct(`Series`) %>% 
  as.character() -> s1



## Replacing the odd value in `Adelaide Hills` with the average of its consecutive numbers ##

AllTS %>% select(s1) %>% filter(`Adelaide Hills` > 80) %>% as.numeric() -> a
which(AllTS$`Adelaide Hills`==a)

AllTS %>% select(`Adelaide Hills`) %>% 
  filter(between(row_number(), 59, 61)) %>% 
  filter(`Adelaide Hills` < 80) %>% 
  summarise(mean(`Adelaide Hills`)) %>% 
  as.numeric() -> b

AllTS_new <- AllTS %>% 
  mutate(`Adelaide Hills` = case_when(`Adelaide Hills`%% a == 0 ~ b, 
                                      TRUE ~ as.double(`Adelaide Hills`)))



# lambda <- numeric(10)
# 
# fc_bias <- numeric(10)
# fc_unbias <- numeric(10)
# for (j in J) {
#   
#   Train <- AllTS[j:(L+j-1),]
#   Test <- AllTS[(L+j):nrow(AllTS),]
#   TS <- Train[,s1] %>% as.ts()
#   TS %>% 
#     autoplot()
#   
#   lambda[j] <- BoxCox.lambda(TS)
#   TS_trans <- BoxCox(TS, lambda = lambda)
#   TS_trans %>% autoplot()
#   
#   fit_bias <- auto.arima(TS, lambda = "auto", biasadj = FALSE)
#   fit_unbiased <- auto.arima(TS, lambda = "auto", biasadj = TRUE)
#   
#   fc_bias[j] <- forecast(fit_bias, h=1, biasadj = FALSE)$mean
#   fc_unbias[j] <-  forecast(fit_unbiased, h=1, biasadj = TRUE)$mean
#   
#   
# }


j <- J[1]
Train <- AllTS_new[j:(L+j-1),]
Test <- AllTS_new[(L+j):nrow(AllTS_new),]
TS <- Train[,s1] %>% as.ts()
TS %>% 
  autoplot()

lambda <- BoxCox.lambda(TS)
TS_trans <- BoxCox(TS, lambda = lambda)
TS_trans %>% autoplot()

fit_bias <- auto.arima(TS, lambda = "auto", biasadj = FALSE)
fit_unbiased <- auto.arima(TS, lambda = "auto", biasadj = TRUE)

fit_bias %>% 
  forecast(h=6, biasadj = FALSE) %>% 
  autoplot()

fit_unbiased %>% 
  forecast(h=6, biasadj = TRUE) %>% 
  autoplot()
