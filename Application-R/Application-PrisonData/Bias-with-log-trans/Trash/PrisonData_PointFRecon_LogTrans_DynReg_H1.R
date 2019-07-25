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
Prison_bot.ts <- read.csv("Prison.bottom.TS.csv")[,-1]

Prison.gts <- gts(Prison_bot.ts, characters = c(3,1,9),
                  gnames = c("State", "Gender", "Legal",
                             "State*Gender", "State*Legal",
                             "Gender*Legal"))


Prison_allts <- allts(Prison.gts)
Prison_allts_log <- apply(Prison_allts, 2, log)
Prison_allts_log_diff <- apply(Prison_allts_log, 2, diff)
# Prison_bot.ts_lag1 <- apply(Prison_bot.ts, 2, lag)
Prison_bot.ts_log <- apply(Prison_bot.ts, 2, log)
Prison_bot.ts_log_diff <- apply(Prison_bot.ts_log, 2, diff)

m <- ncol(Prison_bot.ts)
n <- ncol(Prison_allts)
H <- 1
C <- 20 # number of replications

S <- smatrix(Prison.gts)

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

Fc_backtransformed <- function(Fc, sigma2){
  
  Fc_backtrans <- exp(Fc)
  
  Fc_backtrans_biasadj <- Fc_backtrans*(1 + sigma2/2)
  
  return(rbind(Fc_backtrans = Fc_backtrans, Fc_backtrans_biasadj = Fc_backtrans_biasadj))
  
}



Start <- Sys.time()
for (j in 1:C) {#C
  #j is starting from 2 since we are taking the lag of bot series as regressors and thus the
  #1st element will be ignored
  # For j=3, the models gives weired forecasts. So it was ignored
  
  Train <- Prison_allts[j:(36+j-1),]
  Test <- Prison_allts[(36+j):56,]
  
  Train_log <- Prison_allts_log[j:(36+j-1),]
  Test_log <- Prison_allts_log[(36+j):56,]
  
  Train_log_diff <- Prison_allts_log_diff[j:(35+j-1),]
  Test_log_diff <- Prison_allts_log_diff[(35+j):55,]
  
  Bts_tr <- Prison_bot.ts_log_diff[j:(35+j-1),]
  Bts_tst <- Prison_bot.ts_log_diff[(35+j):55,]
  Names <- colnames(Train)
  
  t <- nrow(Train_log_diff)
  
  if(j==C){
    Test <- as.matrix(Test) %>% t()
  }
  
  Base_forecasts_biased <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  Base_forecasts_unbiased <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  
  Residuals_Transformed <- matrix(NA, nrow = nrow(Train_log_diff), ncol = n)
  Residuals_BackTransformed <- matrix(NA, nrow = nrow(Train_log_diff), ncol = n)
  Residuals_BackTransformed_biasadjust <- matrix(NA, nrow = nrow(Train_log_diff), ncol = n)
  
  Start_fit <- Sys.time()
  
  for (i in 1:(n-m)) {
    #-- For aggregate series we fit univariate ARIMA models with transformation --#
    TS <- Train_log_diff[,i]
    l <- Names[i]
    
    X <- Bts_tr %>%
      as.data.frame() %>%
      as.matrix()
    
    fit <- auto.arima(TS, xreg = X)
    
    xreg_newdata <- Bts_tst %>%
      as.data.frame() %>%
      slice(1:H) %>%
      as.matrix()
    
    Fc <- forecast(fit, h=min(H, nrow(Test)), xreg = xreg_newdata, bootstrap = FALSE)
    Fc_log_diff <- Fc$mean
    L_PI <- Fc$lower[,"95%"]
    sig_h <- (Fc_log_diff - L_PI)/1.96
    sig2_h <- sig_h^2
    
    # sig2_h <- sig_h^2 + c(var(fit$residuals), rep(0, (length(sig_h)-1)))
    # sig2_h <- cumsum(sig2_h)
    
    Fc_log <- numeric(min(H, nrow(Test)))
    
    Fc_log[1] <- Fc_log_diff[1] + Train_log[t,i]
    
    if(length(Fc_log) > 1){
      
      for (p in 2:length(Fc_log)) {
        
        Fc_log[p] <- Fc_log_diff[p] + Fc_log[p-1] 
      }
      
    }
    
    Fc_BackTrans <- mapply(Fc_backtransformed, Fc_log, sigma2 = sig2_h)
    
    Base_forecasts_biased[,i] <- Fc_BackTrans[1,]
    Base_forecasts_unbiased[,i] <- Fc_BackTrans[2,]
    
    #Calculating back transformed and bias adjusted residuals
    Fitted <- fit$fitted
    y_hat_log <- numeric(t)
    
    y_hat_log[1] <- Fitted[1] + Train_log[1,i]
    
    for (p in 2:length(y_hat_log)) {
      
      y_hat_log[p] <- Fitted[p] + y_hat_log[p-1] 
      
    }
    
    
    Fitted_BackTrans <- mapply(Fc_backtransformed, y_hat_log, sigma2 = var(fit$residuals))
    
    
    Residuals_BackTransformed[,i] <- as.vector(Train[2:nrow(Train),i] - Fitted_BackTrans[1,])
    Residuals_BackTransformed_biasadjust[,i] <- as.vector(Train[2:nrow(Train),i] - Fitted_BackTrans[2,])
    
  }
  
  #-- For bottom level series we fit dynamic regression model --#
  
  for(i in (n-m+1):n) {
    
    TS <- Train_log_diff[,i]
    l <- Names[i]
    X <- Bts_tr %>%
      as.data.frame() %>%
      dplyr::select(-l) %>% 
      as.matrix()
    
    fit <- auto.arima(TS, xreg = X)
    
    xreg_newdata <- Bts_tst %>%
      as.data.frame() %>%
      slice(1:H) %>%
      dplyr::select(-l) %>%
      as.matrix()
    
    Fc <- forecast(fit, h=min(H, nrow(Test)), xreg = xreg_newdata, bootstrap = FALSE)
    Fc_log_diff <- Fc$mean
    L_PI <- Fc$lower[,"95%"]
    sig_h <- (Fc_log_diff - L_PI)/1.96
    sig2_h <- sig_h^2
    # sig2_h <- sig_h^2 + c(var(fit$residuals), rep(0, (length(sig_h)-1)))
    # sig2_h <- cumsum(sig2_h)
    
    Fc_log <- numeric(min(H, nrow(Test)))
    
    Fc_log[1] <- Fc_log_diff[1] + Train_log[t,i]
    
    if(length(Fc_log) > 1){
      
      for (p in 2:length(Fc_log)) {
        
        Fc_log[p] <- Fc_log_diff[p] + Fc_log[p-1] 
      }
      
    }
    
    Fc_BackTrans <- mapply(Fc_backtransformed, Fc_log, sigma2 = sig2_h)
    
    Base_forecasts_biased[,i] <- Fc_BackTrans[1,]
    Base_forecasts_unbiased[,i] <- Fc_BackTrans[2,]
    
    #Calculating back transformed and bias adjusted residuals
    Fitted <- fit$fitted
    y_hat_log <- numeric(t)
    
    y_hat_log[1] <- Fitted[1] + Train_log[1,i]
    
    for (p in 2:length(y_hat_log)) {
      
      y_hat_log[p] <- Fitted[p] + y_hat_log[p-1] 
      
    }
    
    
    Fitted_BackTrans <- mapply(Fc_backtransformed, y_hat_log, sigma2 = var(fit$residuals))
    
    
    Residuals_BackTransformed[,i] <- as.vector(Train[2:nrow(Train),i] - Fitted_BackTrans[1,])
    Residuals_BackTransformed_biasadjust[,i] <- as.vector(Train[2:nrow(Train),i] - Fitted_BackTrans[2,])
    
    
  }
  
  End_fit <- Sys.time()
  
  #--Bias correction--#

  Bias <- apply(Residuals_BackTransformed, 2, mean)
  Base_forecasts_unbiased <- Base_forecasts_biased - Bias
  
  
  #--Adding base forecasts to the DF--#
  
  if(j < C){
    Test_1 <- Test[1:min(H, nrow(Test)),]
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
  # DF <- DF %>% add_row(Series = c(colnames(Prison_allts)),
  #                      `F-method` = rep("Least Squares", n),
  #                      `R-method` = "Base",
  #                      Forecasts = Base_forecasts_LS,
  #                      Actual = Test[1,],
  #                      Replication = rep(j, n))
  
  
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

save.image("PrisonData_PointFRecon_LogTrans_DynReg_H1.RData")

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

