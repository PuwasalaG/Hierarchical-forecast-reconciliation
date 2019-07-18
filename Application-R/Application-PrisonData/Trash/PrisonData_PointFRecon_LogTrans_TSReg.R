## This file contains the analysis of Prison data using Hierarchical forecasting methods. 
## Data form a grouped hierarchy with 3 grouping variables - 8-states, 2-Gender, 2-Legal-status. 
## Length of data expands from Q1-2005 to Q4-2018

# For aggregate series we fit time series regression for the log transformed data. 
# Bottom level series are used as the regressors.  
# The back transformed data will be biased and we bias adjust to get unbiased point forecasts.

# For bottom level series we fit time series regression models with log transformations

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
Prison_allts_lag <- apply(Prison_allts, 2, lag)
Prison_bot.ts_lag1 <- apply(Prison_bot.ts, 2, lag)
# Prison_allts_log <- apply(Prison_allts, 2, log)

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
             "Forecasts" = double(),
             "Actual" = double(),
             "Replication" = integer())

Start <- Sys.time()

for (j in c(1,4:20)) {#C 
  #j is starting from 2 since we are taking the lag of bot series as regressors and thus the 
  #1st element will be ignored 
  # For j=3, the models gives weired forecasts. So it was ignored

  Train <- Prison_allts[j:(36+j-1),]
  Train_lag <- Prison_allts_lag[j:(36+j-1),]
  Test <- Prison_allts[(36+j):56,]
  Bts_tr <- Prison_bot.ts[j:(36+j-1),]
  Bts_tst <- Prison_bot.ts[(36+j):56,]
  Bts_lag_tr <- Prison_bot.ts_lag1[j:(36+j-1),]
  Bts_lag_tst <- Prison_bot.ts_lag1[(36+j):56,]
  Names <- colnames(Train)

  Base_forecasts_biased <- numeric(n)
  Base_forecasts_unbiased <- numeric(n)
  
  Residuals_Transformed <- matrix(NA, nrow = nrow(Train), ncol = n)
  Residuals_BackTransformed <- matrix(NA, nrow = nrow(Train), ncol = n)
  Residuals_BackTransformed_biasadjust <- matrix(NA, nrow = nrow(Train), ncol = n)
  

  Start_fit <- Sys.time()
  for (i in 1:(n-m)) {
    
    #-- For aggregate series we fit time series regression models while taking lag of bottom level series as regressors.  --#
    

    Y <- Train[,i]
    Data_log <- data.frame(Y, Bts_tr) %>%
      log() 
    Data_log_lag <- apply(Data_log, 2, lag) %>%
      na.omit() %>% 
      as.ts()
    
    xname <- colnames(Bts_tr)
    fmla <- as.formula(paste("Y ~ ", paste(xname, collapse= "+")))

    tslm_fit <- tslm(formula = fmla, data = Data_log_lag)
    Sigma2 <- var(tslm_fit$residuals, na.rm = TRUE)

    xreg_newdata_log <- Bts_lag_tst %>%
      as.data.frame() %>%
      slice(1:H) %>%
      log()


    Fc <- forecast(tslm_fit, h=H, newdata = xreg_newdata_log)$mean 

    Base_forecasts_biased[i] <- exp(Fc)
    Base_forecasts_unbiased[i] <- exp(Fc)*(1 + 1/2*Sigma2)

    Residuals_BackTransformed[,i] <- as.vector(Y - exp(tslm_fit$fitted.values))
    Residuals_BackTransformed_biasadjust[,i] <- as.vector(Y - exp(tslm_fit$fitted.values))
      
  }
    
  #   #-- For bottom level series we fit time series regression models taking all bottom level series 
  # # excep the dependent series as regressors --#
  #   
  #   for(i in (n-m+1):n) {
  #     
  #     Y <- Train[,i]
  #     l <- Names[i]
  #     X <- Bts_tr %>% 
  #       log() %>% 
  #       as.data.frame() %>% 
  #       dplyr::select(-l) %>% as.matrix()
  #     
  #     Data_log <- data.frame(Y, X) %>%
  #       as.ts()
  #     
  #     xname <- colnames(X)
  #     fmla <- as.formula(paste("Y ~ ", paste(xname, collapse= "+")))
  #     
  #     
  #     tslm_fit <- tslm(formula = fmla, data = Data_log)
  #     Sigma2 <- var(tslm_fit$residuals)
  #     
  #     xreg_newdata_log <- Bts_tst %>%
  #       as.data.frame() %>%
  #       dplyr::select(-l) %>% 
  #       slice(1:H) %>%
  #       log()
  #     
  #     Fc <- forecast(tslm_fit, h=H, newdata = xreg_newdata_log)$mean
  #     
  #     Base_forecasts_biased[i] <- exp(Fc)
  #     Base_forecasts_unbiased[i] <- exp(Fc)*(1 + 1/2*Sigma2)
  #     
  #     Residuals_BackTransformed[,i] <- as.vector(tslm_fit$residuals)
  #     Residuals_BackTransformed_biasadjust[,i] <- as.vector(tslm_fit$residuals)
  #     
  #   }

  #-- For bottom level series we fit dynamic regression model --#
  
  for(i in (n-m+1):n) {
    
    TS <- Train[,i]
    l <- Names[i]
    X <- Bts_tr %>% 
      log() %>% 
      as.data.frame() %>% 
      dplyr::select(-l) %>% as.matrix()
    
    fit_dyn.reg_bias <- auto.arima(TS, xreg = X, lambda = 0, biasadj = FALSE)
    fit_dyn.reg_unbias <- auto.arima(TS, xreg = X, lambda = 0, biasadj = TRUE)
    
    xreg_newdata <- Bts_tst %>% 
      log() %>% 
      as.data.frame() %>% 
      slice(1:H) %>%
      dplyr::select(-l) %>% 
      as.matrix()
    
    # Fc <- forecast(tslm_fit, h=H, xreg = xreg_newdata, lambda = 0, biasadj = TRUE)$mean 
    
    # Data_log <- data.frame(TS, X) %>% log() %>% as.ts()
    # xname <- colnames(X)
    # fmla <- as.formula(paste("Y ~ ", paste(xname, collapse= "+")))
    
    Base_forecasts_biased[i] <- forecast(fit_dyn.reg_bias, h=H, xreg = xreg_newdata, lambda = 0, biasadj = FALSE)$mean
    Base_forecasts_unbiased[i] <- forecast(fit_dyn.reg_unbias, h=H, xreg = xreg_newdata, lambda = 0, biasadj = TRUE)$mean 
    
    # Residuals_BackTransformed[,i] <- as.vector(Y - tslm_fit$fitted)
    # Residuals_BackTransformed_biasadjust[,i] <- as.vector(Y - tslm_fit$fitted)
    
    Residuals_BackTransformed[,i] <- as.vector(TS - fit_dyn.reg_bias$fitted)
    Residuals_BackTransformed_biasadjust[,i] <- as.vector(TS - fit_dyn.reg_unbias$fitted)
    
  }
  
  End_fit <- Sys.time()
  
  Residuals_BackTransformed <- na.omit(Residuals_BackTransformed)
  Residuals_BackTransformed_biasadjust <- na.omit(Residuals_BackTransformed_biasadjust)
  
  
  #--Adding base forecasts to the DF--#
  
  if(j < C){
    Test_1 <- Test[1,]
  } else {
      Test_1 <- as.numeric(Test)
      }
  
  DF <- DF %>% add_row(Series = c(colnames(Prison_allts)), 
                       `F-method` = rep("ARIMA_bias", n), 
                       `R-method` = "Base", 
                       Forecasts = Base_forecasts_biased,
                       Actual = as.numeric(Test_1), 
                       Replication = rep(j, n)) 
  
  DF <- DF %>% add_row(Series = c(colnames(Prison_allts)), 
                       `F-method` = rep("ARIMA_unbiased", n), 
                       `R-method` = "Base", 
                       Forecasts = Base_forecasts_unbiased,
                       Actual = as.numeric(Test_1), 
                       Replication = rep(j, n)) 
  

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
  
  #--Reconciling bias base forecasts--#
  
  Recon_Bias_BU <- S %*% BU_G %*% Base_forecasts_biased
  Recon_Bias_OLS <- S %*% OLS_G %*% Base_forecasts_biased
  Recon_Bias_WLS <- S %*% WLS_G_BackTrans %*% Base_forecasts_biased
  Recon_Bias_MinT <- S %*% MinT.shr_G_BackTrans %*% Base_forecasts_biased
  
  #Follows From Least squars
  Recon_Unbias_BU <- S %*% BU_G %*% Base_forecasts_unbiased
  Recon_Unbias_OLS <- S %*% OLS_G %*% Base_forecasts_unbiased
  Recon_Unbias_WLS <- S %*% WLS_G_BackTrans %*% Base_forecasts_unbiased
  Recon_Unbias_MinT <- S %*% MinT.shr_G_BackTrans %*% Base_forecasts_unbiased
  
  
  
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
  
  Fltr <- DF %>% dplyr::filter(`F-method`=="ARIMA_unbiased", `R-method`=="Base", `Replication`==j) %>% 
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
  group_by(`F-method`, `R-method`) %>% 
  summarise(MSE = mean(SquaredE))

DF %>% 
  group_by(`F-method`, `R-method`, Replication) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  filter(`F-method`=="ARIMA_bias", `R-method`%in% c("Base", "OLS")) %>% 
  ggplot(aes(x = Replication, y = MSE, color = `R-method`)) + 
  geom_line()

DF %>% 
  group_by(`F-method`, `R-method`, Replication) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  filter(`F-method`=="ARIMA_unbiased", `R-method`%in% c("Base", "OLS")) %>% 
  ggplot(aes(x = Replication, y = MSE, color = `R-method`)) + 
  geom_line()

DF %>% 
  group_by(`F-method`, `R-method`, Replication) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  filter(`F-method`=="ARIMA_bias", `R-method`%in% c("Base", "MinT(Shrink)")) %>% 
  ggplot(aes(x = Replication, y = MSE, color = `R-method`)) + 
  geom_line()

  
