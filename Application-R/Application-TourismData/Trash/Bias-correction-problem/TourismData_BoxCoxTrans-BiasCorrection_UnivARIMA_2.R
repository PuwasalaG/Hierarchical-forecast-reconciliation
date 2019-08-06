
# Univariate ARIMA models were fitted for the transformed data (Used BoxCox transformation)
# The back transformed data will be adjusted for bias to get unbiased data


library(tidyverse)
library(fpp2)
library(hts)

# Import bottom level data
#Importing data

OvernightTrips_Region <- read_csv("OvernightTrips_2017.csv")[,-(1:3)]

#generating the hierarchy
Hierarchy <- suppressMessages(hts(OvernightTrips_Region, list(7, c(6,5,4,4,3,3,2), 
                                                              c(2,2,1,4,4,1,3,1,3,6,7,3,4,3,2,3,
                                                                3,4,2,3,1,1,1,2,2,3,4))))
AllTS <- allts(Hierarchy) %>% 
  as_tibble()
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

# A function to return back transformed and back transformed bias adjusted forecasts
# (Given the forecasts in the transformed space)

Fc_backtransformed <- function(Fc, sigma2, lambda){
  
  if(lambda == 0){
    
    Fc_backtrans <- exp(Fc)
    Fc_backtrans_biasadj <- Fc_backtrans*(1 + sigma2/2)
    
  }
  else{
    
    q <- (lambda*Fc + 1)
    
    Fc_backtrans <- q^(1/lambda)
    
    a <- sigma2*(1-lambda)
    b <- 2*q^2
    Fc_backtrans_biasadj <- Fc_backtrans*(1 + a/b)
    
  }
  
  return(rbind(Fc_backtrans = Fc_backtrans, Fc_backtrans_biasadj = Fc_backtrans_biasadj))
  
}

Start <- Sys.time()

for (j in 1:(C-1)) {#C = 140
  
  
  Train <- AllTS[j:(L+j-1),]
  Test <- AllTS[(L+j):nrow(AllTS),]
  Names <- colnames(AllTS)
  
  Base_forecasts_biased <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  Base_forecasts_unbiased <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  
  Residuals_BackTransformed <- matrix(NA, nrow = nrow(Train), ncol = n)
  Residuals_BackTransformed_biasadjust <- matrix(NA, nrow = nrow(Train), ncol = n)
  
  
  Start_fit <- Sys.time()
  
  for (i in 1:n) {
    
    TS <- Train[,i] %>% as.ts()
    lambda <- BoxCox.lambda(TS)
    TS_trans <- BoxCox(TS, lambda = lambda)
    
    fit_trans <- auto.arima(TS_trans) 
    
    Fc <- forecast(fit_trans, h=min(H, nrow(Test)), bootstrap = TRUE)
    Fc_trans <- Fc$mean
    L_PI <- Fc$lower[,"95%"]
    sig_h <- (Fc_trans - L_PI)/1.96
    sig2_h <- sig_h^2
    
    Fc_BackTrans <- mapply(Fc_backtransformed, Fc_trans, sigma2 = sig2_h, 
                           lambda = lambda)
    
    Base_forecasts_biased[,i] <- Fc_BackTrans[1,]
    Base_forecasts_unbiased[,i] <- Fc_BackTrans[2,]
    
    Fitted_BackTrans <- mapply(Fc_backtransformed, fit_trans$fitted, 
                               sigma2 = var(fit_trans$residuals), lambda = lambda)
    
    # 
    # Residuals_BackTransformed[,i] <- as.vector(TS - Fitted_BackTrans[1,])
    # Residuals_BackTransformed_biasadjust[,i] <- as.vector(TS - Fitted_BackTrans[2,])
    
    
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




