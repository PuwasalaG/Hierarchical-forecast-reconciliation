## This file contains the analysis of Prison data using Hierarchical forecasting methods. 
## Data form a grouped hierarchy with 3 grouping variables - 8-states, 2-Gender, 2-Legal-status. 
## Length of data expands from Q1-2005 to Q4-2018

# For aggregate seriesand bottom level series we fit dynamic regression with ARIMA errors for the log transformed data. 
# Bottom level series are used as the regressors.  
# The back transformed data will be biased and we bias adjust to get unbiased point forecasts.


library(tidyverse)
library(fpp2)
library(Matrix)
library(hts)
library(readr)
library(tibble)

# Import bottom level data
Bottom.level_TS <- read.csv("Bottom_level.csv")[,-1]

Hierarchy <- suppressMessages(hts(Bottom.level_TS, list(2, c(2,2))))
AllTS <- allts(Hierarchy)

#Generating the summing matrix
S <- smatrix(Hierarchy)

N <- 750
m <- ncol(Bottom.level_TS)
n <- ncol(AllTS)
L <- 250    # Length of the training set
H <- 3
C <- 500 # number of replications

AllTS <- AllTS %>% 
  as.data.frame() %>% 
  slice(1:N)


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
             "Forecast_Horizon" = integer(),
             "Forecasts" = double(),
             "Actual" = double(),
             "Replication" = integer())

# A function to back transform and bias adjust the forecasts given the forecasts in transformed space
Fc_backtransformed <- function(Fc, lambda, sigma){
  
  Fc_backtrans <- (lambda*Fc +1)^(1/lambda)
  a <- sigma^2*(1-lambda)
  b <- 2*(lambda*Fc_backtrans + 1)^2
  
  Fc_backtrans_biasadj <- Fc_backtrans*(1 + a/b)
  
  return(list(Fc_backtrans, Fc_backtrans_biasadj))

}

# A function to back transform the forecasts given the forecasts in transformed space

FC_BT_biasadjust <- function(Fc, )

##-- Finding the transformation needed for each series --##

lambda <- apply(AllTS, 2, BoxCox.lambda, method = "guerrero")

Start <- Sys.time()
for (j in 1:C) {#C
#j is starting from 2 since we are taking the lag of bot series as regressors and thus the
#1st element will be ignored
# For j=3, the models gives weired forecasts. So it was ignored
  
  Train <- AllTS[j:(L+j-1),]
  Test <- AllTS[(L+j):750,]
  Bts_tr <- Bottom.level_TS[j:(L+j-1),]
  Bts_tst <- Bottom.level_TS[(L+j):750,]
  Names <- colnames(Train)
  
  if(j==C){
    Test <- as.matrix(Test) %>% t()
  }
  
  Base_forecasts_biased <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  Base_forecasts_unbiased <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  Residuals_Transformed <- matrix(NA, nrow = nrow(Train), ncol = n)
  Residuals_BackTransformed <- matrix(NA, nrow = nrow(Train), ncol = n)
  Residuals_BackTransformed_biasadjust <- matrix(NA, nrow = nrow(Train), ncol = n)
  
  Start_fit <- Sys.time()
  
  for (i in 1:n) {
    
    ##-- For top level series, all bottom level series are taken as exogenous variables --#
    
    if(i==1){
      
      TS <- Train[,i]
      lam <- 0.5
      TS_Trans <- BoxCox(TS, lambda = lam)
      l <- Names[i]
      
      X <- Bts_tr %>%
        as.data.frame() %>%
        as.matrix()
      
      fit <- auto.arima(TS_Trans, xreg = X)
      # fit_dyn.reg_unbias <- auto.arima(TS, xreg = X, lambda = 0.5, biasadj = TRUE)
      
      xreg_newdata <- Bts_tst %>%
        as.data.frame() %>%
        slice(1:min(H, nrow(Test))) %>%
        as.matrix()
      
      Fc <- forecast(fit, h=min(H, nrow(Test)), xreg = xreg_newdata)
      Base_Fc_trans <- Fc$mean
      U_PI <- Fc$upper[,"95%"]
      sig_h <- (U_PI - Base_Fc_trans)/1.96
      
      Fc_BackTrans <- mapply(Fc_backtransformed, Base_Fc_trans, lambda = lam, sigma = sig_h)
      
      Base_forecasts_biased[, i] <- mapply(Fc_backtransformed, Base_Fc_trans, lambda = lam)
      Base_forecasts_unbiased[,i] <- forecast(fit_dyn.reg_unbias, h=min(H, nrow(Test)), 
                                              xreg = xreg_newdata, lambda = 0.5, biasadj = TRUE)$mean
      
      Residuals_BackTransformed[,i] <- as.vector(TS - fit_dyn.reg_bias$fitted)
      Residuals_BackTransformed_biasadjust[,i] <- as.vector(TS - fit_dyn.reg_unbias$fitted)
      
      
    }
    
    ##-- For middle level series, their children series are taken as exogenous variables --#
    
    if(i == 2){
      
      TS <- Train[,i]
      Bot_names <- Names[4:5]
      
      X <- Bts_tr %>%
        select(Bot_names) %>%
        as.data.frame() %>%
        as.matrix()
      
      fit_dyn.reg_bias <- auto.arima(TS, xreg = X, lambda = 0.5, biasadj = FALSE)
      fit_dyn.reg_unbias <- auto.arima(TS, xreg = X, lambda = 0.5, biasadj = TRUE)
      
      xreg_newdata <- Bts_tst %>%
        select(Bot_names) %>%
        as.data.frame() %>%
        slice(1:H) %>%
        as.matrix()
      
      Base_forecasts_biased[,i] <- forecast(fit_dyn.reg_bias, h=min(H, nrow(Test)), 
                                            xreg = xreg_newdata, lambda = 0.5, biasadj = FALSE)$mean
      Base_forecasts_unbiased[,i] <- forecast(fit_dyn.reg_unbias, h=min(H, nrow(Test)), 
                                              xreg = xreg_newdata, lambda = 0.5, biasadj = TRUE)$mean
      
      Residuals_BackTransformed[,i] <- as.vector(TS - fit_dyn.reg_bias$fitted)
      Residuals_BackTransformed_biasadjust[,i] <- as.vector(TS - fit_dyn.reg_unbias$fitted)
      
      
    }
    
    if(i == 3){
      
      TS <- Train[,i]
      Bot_names <- Names[6:7]
      
      X <- Bts_tr %>%
        select(Bot_names) %>%
        as.data.frame() %>%
        as.matrix()
      
      fit_dyn.reg_bias <- auto.arima(TS, xreg = X, lambda = 0.5, biasadj = FALSE)
      fit_dyn.reg_unbias <- auto.arima(TS, xreg = X, lambda = 0.5, biasadj = TRUE)
      
      xreg_newdata <- Bts_tst %>%
        select(Bot_names) %>%
        as.data.frame() %>%
        slice(1:H) %>%
        as.matrix()
      
      Base_forecasts_biased[,i] <- forecast(fit_dyn.reg_bias, h=min(H, nrow(Test)), 
                                            xreg = xreg_newdata, lambda = 0.5, biasadj = FALSE)$mean
      Base_forecasts_unbiased[,i] <- forecast(fit_dyn.reg_unbias, h=min(H, nrow(Test)), 
                                              xreg = xreg_newdata, lambda = 0.5, biasadj = TRUE)$mean
      
      Residuals_BackTransformed[,i] <- as.vector(TS - fit_dyn.reg_bias$fitted)
      Residuals_BackTransformed_biasadjust[,i] <- as.vector(TS - fit_dyn.reg_unbias$fitted)
      
      
    }
    
    ##-- For bottom level series, all other bottom level series are taken as exogenous variables --#
    
    if(i %in% c(4:7)){
      
      TS <- Train[,i]
      l <- Names[i]
      
      X <- Bts_tr %>%
        as.data.frame() %>%
        dplyr::select(-l) %>% 
        as.matrix()
      
      fit_dyn.reg_bias <- auto.arima(TS, xreg = X, lambda = 0.5, biasadj = FALSE)
      fit_dyn.reg_unbias <- auto.arima(TS, xreg = X, lambda = 0.5, biasadj = TRUE)
      
      xreg_newdata <- Bts_tst %>%
        as.data.frame() %>%
        slice(1:H) %>%
        dplyr::select(-l) %>%
        as.matrix()
      
      Base_forecasts_biased[,i] <- forecast(fit_dyn.reg_bias, h=min(H, nrow(Test)), 
                                            xreg = xreg_newdata, lambda = 0.5, biasadj = FALSE)$mean
      Base_forecasts_unbiased[,i] <- forecast(fit_dyn.reg_unbias, h=min(H, nrow(Test)), 
                                              xreg = xreg_newdata, lambda = 0.5, biasadj = TRUE)$mean
      
      Residuals_BackTransformed[,i] <- as.vector(TS - fit_dyn.reg_bias$fitted)
      Residuals_BackTransformed_biasadjust[,i] <- as.vector(TS - fit_dyn.reg_unbias$fitted)
      
      
    }
    

  }
  
  End_fit <- Sys.time()
  
  #--Adding base forecasts to the DF--#
  
  if(j < C){
  Test_1 <- Test[1:min(H, nrow(Test)),] %>% as.matrix()
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
  
  # WLS_BackTrans G (This G matrix is calculated using back-transformed residuals)
  SamCov_BackTrans <- cov(Residuals_BackTransformed)
  Inv_SamCov_BackTrans <- diag(1/diag(SamCov_BackTrans), n, n)
  WLS_G_BackTrans <- solve(t(S) %*% Inv_SamCov_BackTrans %*% S) %*% t(S) %*% Inv_SamCov_BackTrans
  
  # WLS_BT_biasadjust G (This G matrix is calculated using back-transformed, bias adjusted residuals)
  SamCov_BT_biasadjust <- cov(Residuals_BackTransformed_biasadjust)
  Inv_SamCov_BT_biasadjust <- diag(1/diag(SamCov_BT_biasadjust), n, n)
  WLS_G_BT_biasadjust <- solve(t(S) %*% Inv_SamCov_BT_biasadjust %*% S) %*% t(S) %*% Inv_SamCov_BT_biasadjust
  
  # MinT(Shrink)_BackTrans G (This G matrix is calculated using back-transformed residuals)
  targ_BackTrans <- lowerD(Residuals_BackTransformed)
  shrink_BackTrans <- shrink.estim(Residuals_BackTransformed, targ_BackTrans)
  Shr.cov_BackTrans <- shrink_BackTrans[[1]]
  Inv_Shr.cov_BackTrans <- qr.solve(Shr.cov_BackTrans)
  MinT.shr_G_BackTrans <- solve(t(S) %*% Inv_Shr.cov_BackTrans %*% S) %*% t(S) %*%
  Inv_Shr.cov_BackTrans
  
  # MinT(Shrink)_BT_biasadjust G (This G matrix is calculated using back-transformed, bias adjusted residuals)
  targ_BT_biasadjust <- lowerD(Residuals_BackTransformed_biasadjust)
  shrink_BT_biasadjust <- shrink.estim(Residuals_BackTransformed_biasadjust, targ_BT_biasadjust)
  Shr.cov_BT_biasadjust <- shrink_BT_biasadjust[[1]]
  Inv_Shr.cov_BT_biasadjust <- solve(Shr.cov_BT_biasadjust)
  MinT.shr_G_BT_biasadjust <- solve(t(S) %*% Inv_Shr.cov_BT_biasadjust %*% S) %*% t(S) %*%
  Inv_Shr.cov_BT_biasadjust
  
  #--Reconciling bias base forecasts--#
  Recon_Bias_BU <- t(S %*% BU_G %*% t(Base_forecasts_biased))
  Recon_Bias_OLS <- t(S %*% OLS_G %*% t(Base_forecasts_biased))
  Recon_Bias_WLS <- t(S %*% WLS_G_BackTrans %*% t(Base_forecasts_biased))
  Recon_Bias_MinT <- t(S %*% MinT.shr_G_BackTrans %*% t(Base_forecasts_biased))
  
  #Follows From bias adjusted forecasts
  Recon_Unbias_BU <- t(S %*% BU_G %*% t(Base_forecasts_unbiased))
  Recon_Unbias_OLS <- t(S %*% OLS_G %*% t(Base_forecasts_unbiased))
  Recon_Unbias_WLS <- t(S %*% WLS_G_BackTrans %*% t(Base_forecasts_unbiased))
  Recon_Unbias_MinT <- t(S %*% MinT.shr_G_BackTrans %*% t(Base_forecasts_unbiased))
  
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

DF %>% 
  group_by(`F-method`, `R-method`, Replication, Forecast_Horizon) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  filter(`F-method`=="ARIMA_unbias", `R-method`%in% c("Base", "MinT(Shrink)")) %>% 
  ggplot(aes(x = Replication, y = MSE, color = `R-method`)) + 
  geom_line() + 
  facet_wrap( ~ Forecast_Horizon, scales = "free_y")

