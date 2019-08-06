## This file contains the analysis of Tourism data using Hierarchical forecasting methods. 
## Data form a hierarchy with 3 levels - 8-states, -Zones, -Regions. 
## Length of data expands from Jan-1998 to Dec-2017

# Here we fit univariate ARIMA models for the log transformed data
# The back transformed data will be biased and we bias adjust to get 
# unbiased point forecasts.


library(tidyverse)
library(fpp2)
library(Matrix)
library(hts)
library(readr)
library(tibble)

# Import bottom level data
#Importing data

OvernightTrips_Region <- read_csv("OvernightTrips_2017.csv")[,-(1:3)]

#generating the hierarchy
#Hierarchy<-suppressMessages(hts(Bottom_level, list(7, c(14,21,12,12,5,5,7))))
Hierarchy <- suppressMessages(hts(OvernightTrips_Region, list(7, c(6,5,4,4,3,3,2), 
                                                            c(2,2,1,4,4,1,3,1,3,6,7,3,4,3,2,3,
                                                              3,4,2,3,1,1,1,2,2,3,4))))
AllTS <- allts(Hierarchy) %>% 
  as_tibble()
n <- ncol(AllTS)
l1 <- 1
l2 <- sum(7)
l3 <- sum(6,5,4,4,3,3,2)
m <- ncol(OvernightTrips_Region)
H <- 6
L <- 100 #Size of the training sample 
C <- nrow(AllTS) - L


#Generating the summing matrix
S <- smatrix(Hierarchy)
#Code to get shrinkage estimator

lowerD <- function(x)
{
  n2 <- nrow(x)
  return(diag(apply(x, 2, crossprod) / n2))
}

shrink.estim <- function(x, tar)
{
  if (is.matrix(x) == TRUE && is.numeric(x) == FALSE)
    stop("The data matrix must be numeric!")
  p1 <- ncol(x)
  n2 <- nrow(x)
  covm <- crossprod(x) / n2
  corm <- cov2cor(covm)
  xs <- scale(x, center = FALSE, scale = sqrt(diag(covm)))
  v <- (1/(n2 * (n2 - 1))) * (crossprod(xs^2) - 1/n2 * (crossprod(xs))^2)
  diag(v) <- 0
  corapn <- cov2cor(tar)
  d <- (corm - corapn)^2
  lambda <- sum(v)/sum(d)
  lambda <- max(min(lambda, 1), 0)
  shrink.cov <- lambda * tar + (1 - lambda) * covm
  return(list(shrink.cov, c("The shrinkage intensity lambda is:",
                            round(lambda, digits = 4))))
}

#To store results
DF <- tibble("Series" = character(),
             "F-method" = character(),
             "R-method" = character(),
             "Forecast_Horizon" = double(),
             "Forecasts" = double(),
             "Actual" = double(),
             "Replication" = integer())

# A function to return back transformed forecasts and bias adjust back transformed forecasts
# given the forecasts in the transformed space

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

for (j in 1:C) {#C = 140
  

  Train <- AllTS[j:(L+j-1),]
  Test <- AllTS[(L+j):nrow(AllTS),]
  
  if(j==C){
    Test <- as.matrix(Test) %>% t()
  }
  Names <- colnames(AllTS)

  Base_forecasts_biased <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  Base_forecasts_unbiased <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  
  Residuals_Transformed <- matrix(NA, nrow = nrow(Train), ncol = n)
  Residuals_BackTransformed <- matrix(NA, nrow = nrow(Train), ncol = n)
  Residuals_BackTransformed_biasadjust <- matrix(NA, nrow = nrow(Train), ncol = n)
 
  
  Start_fit <- Sys.time()
  
  for (i in 1:n) {
    
    TS <- Train[,i] %>% as.ts()
    TS <- TS+1 # adding 1 since some series has zero values which is problamatic when taking log transformation
    
    lambda <- 0
    TS_trans <- BoxCox(TS, lambda = lambda)
    
    fit_trans <- auto.arima(TS_trans) 
    
    Fc <- forecast(fit_trans, h=min(H, nrow(Test)))
    Fc_trans <- Fc$mean
    L_PI <- Fc$lower[,"95%"]
    sig_h <- (Fc_trans - L_PI)/1.96
    sig2_h <- sig_h^2

    Fc_BackTrans <- mapply(Fc_backtransformed, Fc_trans, sigma2 = sig2_h, 
                           lambda = lambda)
    
    Base_forecasts_biased[,i] <- Fc_BackTrans[1,] - 1 #removing the 1 we added initially
    Base_forecasts_unbiased[,i] <- Fc_BackTrans[2,] - 1 #removing the 1 we added initially
    
    Fitted_BackTrans <- mapply(Fc_backtransformed, fit_trans$fitted, 
                               sigma2 = var(fit_trans$residuals), lambda = lambda) - 1 #removing the 1 we added initially
    
    Residuals_BackTransformed[,i] <- as.vector(TS - Fitted_BackTrans[1,])
    Residuals_BackTransformed_biasadjust[,i] <- as.vector(TS - Fitted_BackTrans[2,])
    
    
  }
  
  End_fit <- Sys.time()
  
  # #--Bias correction of residuals--#
  # 
  # Bias <- apply(Residuals_BackTransformed, 2, mean)
  # # Base_forecasts_unbiased <- Base_forecasts_biased - Bias
  # 
  # Bias_mat <- matrix(rep(Bias, nrow(Train)), nrow = nrow(Train), ncol = n, byrow = T)
  # Residuals_BackTransformed_biasadjust <- Residuals_BackTransformed - Bias_mat
  
  

  
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
  
  
  ###--Reconciliation--###
  
  #--Calculating G matrices--#
  
  # Bottom-up G
  
  Null.ma <- matrix(0,m,(n-m))
  BU_G <- cbind(Null.ma, diag(1,m,m))
  
  # OLS G
  
  OLS_G <- solve(t(S) %*% S) %*% t(S)
  
  # # WLS_BackTrans G (This G matrix is calculated using back-transformed residuals)
  # 
  # SamCov_BackTrans <- cov(Residuals_BackTransformed)
  # Inv_SamCov_BackTrans <- diag(1/diag(SamCov_BackTrans), n, n)
  # 
  # WLS_G_BackTrans <- solve(t(S) %*% Inv_SamCov_BackTrans %*% S) %*% t(S) %*% Inv_SamCov_BackTrans
  # 
  # # WLS_BT_biasadjust G (This G matrix is calculated using back-transformed, bias adjusted residuals)
  # 
  # SamCov_BT_biasadjust <- cov(Residuals_BackTransformed_biasadjust)
  # Inv_SamCov_BT_biasadjust <- diag(1/diag(SamCov_BT_biasadjust), n, n)
  # 
  # WLS_G_BT_biasadjust <- solve(t(S) %*% Inv_SamCov_BT_biasadjust %*% S) %*% t(S) %*% Inv_SamCov_BT_biasadjust
  
  
  # MinT(Shrink)_BackTrans G (This G matrix is calculated using back-transformed residuals)
  
  targ_BackTrans <- lowerD(Residuals_BackTransformed)
  shrink_BackTrans <- shrink.estim(Residuals_BackTransformed, targ_BackTrans)
  Shr.cov_BackTrans <- shrink_BackTrans[[1]]
  Inv_Shr.cov_BackTrans <- solve(Shr.cov_BackTrans)
  
  MinT.shr_G_BackTrans <- solve(t(S) %*% Inv_Shr.cov_BackTrans %*% S) %*% t(S) %*% 
    Inv_Shr.cov_BackTrans
  
  # MinT(Shrink)_BT_biasadjust G (This G matrix is calculated using back-transformed, bias adjusted residuals)
  
  targ_BT_biasadjust <- lowerD(Residuals_BackTransformed_biasadjust)
  shrink_BT_biasadjust <- shrink.estim(Residuals_BackTransformed_biasadjust, targ_BT_biasadjust)
  Shr.cov_BT_biasadjust <- shrink_BT_biasadjust[[1]]
  Inv_Shr.cov_BT_biasadjust <- solve(Shr.cov_BT_biasadjust)
  
  MinT.shr_G_BT_biasadjust <- solve(t(S) %*% Inv_Shr.cov_BT_biasadjust %*% S) %*% t(S) %*% 
    Inv_Shr.cov_BT_biasadjust
  
  # WLS_BackTrans G (This G matrix is calculated using back-transformed residuals)
  
  Inv_WLS_BackTrans <- diag(1/diag(Shr.cov_BackTrans), n, n)
  
  WLS_G_BackTrans <- solve(t(S) %*% Inv_WLS_BackTrans %*% S) %*% t(S) %*% 
    Inv_WLS_BackTrans
  
  # WLS_BT_biasadjust G (This G matrix is calculated using back-transformed, bias adjusted residuals)
  
  Inv_WLS_BT_biasadjust <- diag(1/diag(Shr.cov_BT_biasadjust), n, n)
  
  WLS_G_BT_biasadjust <- solve(t(S) %*% Inv_WLS_BT_biasadjust %*% S) %*% t(S) %*% 
    Inv_WLS_BT_biasadjust
  
  
  #--Reconciling bias base forecasts--#
  
  Recon_Bias_BU <- t(S %*% BU_G %*% t(Base_forecasts_biased))
  Recon_Bias_OLS <- t(S %*% OLS_G %*% t(Base_forecasts_biased))
  Recon_Bias_WLS <- t(S %*% WLS_G_BackTrans %*% t(Base_forecasts_biased))
  Recon_Bias_MinT <- t(S %*% MinT.shr_G_BackTrans %*% t(Base_forecasts_biased))
  
  #Follows From bias adjusted forecasts
  Recon_Unbias_BU <- t(S %*% BU_G %*% t(Base_forecasts_unbiased))
  Recon_Unbias_OLS <- t(S %*% OLS_G %*% t(Base_forecasts_unbiased))
  Recon_Unbias_WLS <- t(S %*% WLS_G_BT_biasadjust %*% t(Base_forecasts_unbiased))
  Recon_Unbias_MinT <- t(S %*% MinT.shr_G_BT_biasadjust %*% t(Base_forecasts_unbiased))
  
  
  
  #--Adding the reconcilied forecasts from biased base forecasts to the DF--#
  
  Fltr <- DF %>% dplyr::filter(`F-method`=="ARIMA_bias", `R-method`=="Base", `Replication`==j) %>% 
    dplyr::select(-Forecasts, -`R-method`)
  
  cbind(Fltr, Forecasts = as.vector(Recon_Bias_BU), "R-method" = "Bottom-up") -> Df_BU
  Df_BU[names(DF)] -> Df_BU
  DF <- rbind(DF, Df_BU)
  
  cbind(Fltr, Forecasts = as.vector(Recon_Bias_OLS), "R-method" = "OLS") -> Df_OLS
  Df_OLS[names(DF)] -> Df_OLS
  DF <- rbind(DF, Df_OLS)
  
  cbind(Fltr, Forecasts = as.vector(Recon_Bias_WLS), "R-method" = "WLS") -> Df_WLS
  Df_WLS[names(DF)] -> Df_WLS
  DF <- rbind(DF, Df_WLS)
  
  cbind(Fltr, Forecasts = as.vector(Recon_Bias_MinT), "R-method" = "MinT(Shrink)") -> Df_MShr
  Df_MShr[names(DF)] -> Df_MShr
  DF <- rbind(DF, Df_MShr)
  
  
  
  #--Adding the reconcilied forecasts from unbiased base forecasts to the DF--#
  
  Fltr <- DF %>% dplyr::filter(`F-method`=="ARIMA_unbias", `R-method`=="Base", `Replication`==j) %>% 
    dplyr::select(-Forecasts, -`R-method`)
  
  cbind(Fltr, Forecasts = as.vector(Recon_Unbias_BU), "R-method" = "Bottom-up") -> Df_BU
  Df_BU[names(DF)] -> Df_BU
  DF <- rbind(DF, Df_BU)
  
  cbind(Fltr, Forecasts = as.vector(Recon_Unbias_OLS), "R-method" = "OLS") -> Df_OLS
  Df_OLS[names(DF)] -> Df_OLS
  DF <- rbind(DF, Df_OLS)
  
  cbind(Fltr, Forecasts = as.vector(Recon_Unbias_WLS), "R-method" = "WLS") -> Df_WLS
  Df_WLS[names(DF)] -> Df_WLS
  DF <- rbind(DF, Df_WLS)
  
  cbind(Fltr, Forecasts = as.vector(Recon_Unbias_MinT), "R-method" = "MinT(Shrink)") -> Df_MShr
  Df_MShr[names(DF)] -> Df_MShr
  DF <- rbind(DF, Df_MShr)
  
  
}

End <- Sys.time()

save.image("TourismData_LogTransBiasCorrection_UnivARIMA.RData")

DF %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF

DF %>% 
  group_by(`F-method`, `R-method`, Forecast_Horizon) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  spread(key = Forecast_Horizon, value = MSE)

DF %>% 
  group_by(`F-method`, `R-method`, Replication, Forecast_Horizon) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  filter(`F-method`=="ARIMA_bias", `R-method`%in% c("Base", "OLS")) %>% 
  ggplot(aes(x = Replication, y = MSE, color = `R-method`)) + 
  geom_line() + 
  facet_wrap( ~ Forecast_Horizon, scales = "free_y")

DF %>% 
  group_by(`F-method`, `R-method`, Replication, Forecast_Horizon) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  filter(`F-method`=="ARIMA_unbias", `R-method`%in% c("Base", "OLS")) %>% 
  ggplot(aes(x = Replication, y = MSE, color = `R-method`)) + 
  geom_line() + 
  facet_wrap( ~ Forecast_Horizon, scales = "free_y")

DF %>% 
  group_by(`F-method`, `R-method`, Replication, Forecast_Horizon) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  filter(`F-method`=="ARIMA_bias", `R-method`%in% c("Base", "MinT(Shrink)")) %>% 
  ggplot(aes(x = Replication, y = MSE, color = `R-method`)) + 
  geom_line() + 
  facet_wrap( ~ Forecast_Horizon, scales = "free_y")


## Box-plots for the differences of MSE between OLS, MinT vs the base

DF_MSE <- DF %>% 
  group_by(`F-method`, `R-method`, Replication, Forecast_Horizon) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  mutate(Forecast_Horizon = recode(Forecast_Horizon, "1" = "h=1", "2" = "h=2", "3"= "h=3", "4" = "h=4"))


DF_MSE %>% 
  filter(`F-method`=="ARIMA_unbias", `R-method`%in% c("Base", "OLS", "MinT(Shrink)")) %>% 
  spread(key = `R-method`, value = MSE) %>% 
  mutate("Base-OLS" = Base - OLS,
         "Base-MinT" = Base - `MinT(Shrink)` ) %>% 
  select(Replication, Forecast_Horizon, `Base-OLS`, `Base-MinT`) %>% 
  gather(`Base-MinT`, `Base-OLS`, key = Method, value = MSE) %>% 
  ggplot(aes(x = Method, y = MSE)) + geom_boxplot() + facet_wrap(~`Forecast_Horizon`)
  
  
  

