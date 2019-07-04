## This file contains the analysis of Prison data using Hierarchical forecasting methods. 
## Data form a grouped hierarchy with 3 grouping variables - 8-states, 2-Gender, 2-Legal-status. 
## Length of data expands from Q1-2005 to Q4-2018

## Two types of models were fitted. 
#   1. VARX models with lasso. Here we take all lag of bottom level series as exogeneous 
#      variables for all aggregate levels.
#      All bottom level series are fitted as a VAR(1) system           

#   2. VARX models with least squares. Here we take all lag of bottom level series as exogeneous 
#      variables for all aggregate levels.
#      All bottom level series are fitted as a VAR(1) system           


library(tidyverse)
library(fpp2)
library(Matrix)
library(hts)
library(readr)
library(BigVAR)
library(vars)
library(tibble)

# Import bottom level data
Prison_bot.ts <- read.csv("Prison.bottom.TS.csv")[,-1]

Prison.gts <- gts(Prison_bot.ts/1e3, characters = c(3,1,9),
                  gnames = c("State", "Gender", "Legal",
                             "State*Gender", "State*Legal",
                             "Gender*Legal"))


Prison_allts <- allts(Prison.gts)

m <- ncol(Prison_bot.ts)
n <- ncol(Prison_allts)
p <- 1 #order of the VAR
H <- 1
C <- 16 # number of replications

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
for (j in 1:15) {#C
  
  
  Train <- Prison_allts[j:(40+j-1),]
  Test <- Prison_allts[(40+j):56,]
  Bts_tr <- Train[,(n-m+1):n]
  

  Base_forecasts_agg_LASSO <- numeric((n-m))
  Base_forecasts_agg_LS <- numeric((n-m))
  
  Residual_agg_Lasso <- matrix(NA, nrow = nrow(Train)-1, ncol = (n-m))
  Residual_agg_LS <- matrix(NA, nrow = nrow(Train)-1, ncol = (n-m))
  
  #--For aggregate series--##
  
  for (i in 1:(n-m)) {
    
    y <- Train[,i]
    y_1 <- lag(y, 1)[-1]
    
    B_diff <- diff(Bts_tr, lag = 1)
    
    B_diff_1 <- matrix(0, nrow = nrow(Train)-1, ncol = m)
    
    for (l in 1:m) {
      
      B_diff_1[,l] <- lag(B_diff[,l], 1)
      
    }
    
    colnames(B_diff_1) <- colnames(Bts_tr)
    
    
    y_diff1 <- diff(y, lag = 1)
    
    y_diff1_1 <- lag(y_diff1, 1)
    y_diff1_2 <- lag(y_diff1, 2)
    y_diff1_3 <- lag(y_diff1, 3)
    y_diff1_4 <- lag(y_diff1, 4)
    
    y1 <- y_diff1_1[-(1:4)]
    y2 <- y_diff1_2[-(1:4)]
    y3 <- y_diff1_3[-(1:4)]
    y4 <- y_diff1_4[-(1:4)]
    X <- data.frame(y1, y2, y3, y4, B_diff_1[-(1:4),]) %>% as.matrix()
    
    Y <- y_diff1[-(1:4)]
    
    
    ##--LASSO--##
    
    Lasso_fit <- glmnet(x = X, y = Y, family = c("gaussian"), alpha = 1)
    
    CV_lasso <- cv.glmnet(x = X, y = Y, family = "gaussian")
    opt.lambda <- CV_lasso$lambda.min
    
    Pars_lasso <- coef(Lasso_fit, s=opt.lambda) %>% as.matrix()
    
    ##--Fitted values and residuals from LASSO
    Intercept <- rep(1, nrow(B_diff_1))
    X1 <- data.frame(Intercept, y_diff1_1, y_diff1_2, y_diff1_3, y_diff1_4, B_diff_1) %>% 
      as.matrix()
    X1[is.na(X1)] <- 0
    y_fitted_lasso <- t(Pars_lasso) %*% t(X1) + y_1
    
    Residual_agg_Lasso[,i] <- y[-1] - y_fitted_lasso
    
    ##--Forecasts from LASSO
    
    t <- length(y)
    const_lasso <- Pars_lasso[1]
    alpha_lasso <- Pars_lasso[2:5]
    beta_lasso <- Pars_lasso[6:37]
    
    
    Base_forecasts_agg_LASSO[i] <- const_lasso + y[t] + alpha_lasso[1]*(y[t]-y[t-1]) + 
      alpha_lasso[2]*(y[t-1] - y[t-2]) + alpha_lasso[3]*(y[t-2] - y[t-3]) + 
      alpha_lasso[4]*(y[t-3] - y[t-4]) + beta_lasso %*% (Bts_tr[(t),] - Bts_tr[(t-1),])
    
    ##--Least Squares--##
    
    # Data <- data.frame(Y, X) %>% as.ts()
    # xnam <- colnames(Data)[2:37]
    # fmla <- as.formula(paste("Y ~ ", paste(xnam, collapse= "+")))
    # LS_fit <- tslm(formula = fmla, data = Data)
    # Pars_LS <- coefficients(LS_fit) 
    # Pars_LS[is.na(Pars_LS)] <- 0
    
    LS_fit <- glmnet(x = X, y = Y, family = c("gaussian"), alpha = 1, lambda = 0)
    Pars_LS <- coefficients(LS_fit, s=0) 

    
    ##--Fitted values and residuals from Least squares
    
    y_fitted_LS <- t(Pars_LS) %*% t(X1) + y_1
    Residual_agg_LS[,i] <- (y[-1] - y_fitted_LS) %>% as.numeric()
    
    ##--Forecasts from least squares
    const_LS <- Pars_LS[1]
    alpha_LS <- Pars_LS[2:5]
    beta_LS <- Pars_LS[6:37]
    
    Base_forecasts_agg_LS[i] <- const_LS + y[t] + alpha_LS[1]*(y[t]-y[t-1]) + 
      alpha_LS[2]*(y[t-1] - y[t-2]) + alpha_LS[3]*(y[t-2] - y[t-3]) + 
      alpha_LS[4]*(y[t-3] - y[t-4]) + beta_LS %*% (Bts_tr[(t),] - Bts_tr[(t-1),])
    
    
  } 
  
  
  #--For disaggregate series--##
  
  Base_forecasts_bot_LASSO <- numeric(m)
  Base_forecasts_bot_LS <- numeric(m)
  
  Residual_bot_Lasso <- matrix(NA, nrow = nrow(Train)-1, ncol = m)
  Residual_bot_LS <- matrix(NA, nrow = nrow(Train)-1, ncol = m)
  
  
  for (i in 1:m) {
    
    y <- Bts_tr[,i]
    y_1 <- lag(y, 1)[-1]
    
    B <- Bts_tr[,-i]
    
    B_diff <- diff(B, lag = 1)
    
    B_diff_1 <- matrix(0, nrow = nrow(Train)-1, ncol = m-1)
    
    for (l in 1:(m-1)) {
      
      B_diff_1[,l] <- lag(B_diff[,l], 1)
      
    }
    
    colnames(B_diff_1) <- colnames(B)
    
    
    y_diff1 <- diff(y, lag = 1)

    y_diff1_1 <- lag(y_diff1, 1)
    y_diff1_2 <- lag(y_diff1, 2)
    y_diff1_3 <- lag(y_diff1, 3)
    y_diff1_4 <- lag(y_diff1, 4)
    
    y1 <- y_diff1_1[-(1:4)]
    y2 <- y_diff1_2[-(1:4)]
    y3 <- y_diff1_3[-(1:4)]
    y4 <- y_diff1_4[-(1:4)]
    X <- data.frame(y1, y2, y3, y4, B_diff_1[-(1:4),]) %>% as.matrix()
    
    Y <- y_diff1[-(1:4)]
    
    
    ##--LASSO--##
    
    Lasso_fit <- glmnet(x = X, y = Y, family = c("gaussian"), alpha = 1)
    
    CV_lasso <- cv.glmnet(x = X, y = Y, family = "gaussian")
    opt.lambda <- CV_lasso$lambda.min
    
    Pars_lasso <- coef(Lasso_fit, s=opt.lambda) %>% as.matrix()
    
    ##--Fitted values and residuals from LASSO
    Intercept <- rep(1, nrow(B_diff_1))
    X1 <- data.frame(Intercept, y_diff1_1, y_diff1_2, y_diff1_3, y_diff1_4, B_diff_1) %>% 
      as.matrix()
    X1[is.na(X1)] <- 0
    y_fitted_lasso <- t(Pars_lasso) %*% t(X1) + y_1
    
    Residual_bot_Lasso[,i] <- y[-1] - y_fitted_lasso
    
    ##--Forecasts from LASSO
    
    t <- length(y)
    const_lasso <- Pars_lasso[1]
    alpha_lasso <- Pars_lasso[2:5]
    beta_lasso <- Pars_lasso[6:36]
    
    
    Base_forecasts_bot_LASSO[i] <- const_lasso + y[t] + alpha_lasso[1]*(y[t]-y[t-1]) + 
      alpha_lasso[2]*(y[t-1] - y[t-2]) + alpha_lasso[3]*(y[t-2] - y[t-3]) + 
      alpha_lasso[4]*(y[t-3] - y[t-4]) + beta_lasso %*% (B[(t),] - B[(t-1),])
    
    ##--Least Squares--##
    
    # Data <- data.frame(Y, X) %>% as.ts()
    # xnam <- colnames(Data)[2:36]
    # fmla <- as.formula(paste("Y ~ ", paste(xnam, collapse= "+")))
    # LS_fit <- tslm(formula = fmla, data = Data)
    # Pars_LS <- coefficients(LS_fit) 
    # Pars_LS[is.na(Pars_LS)] <- 0
    
    LS_fit <- glmnet(x = X, y = Y, family = c("gaussian"), alpha = 1, lambda = 0)
    Pars_LS <- coefficients(LS_fit, s=0) 
    
    ##--Fitted values and residuals from Least squares
    
    y_fitted_LS <- t(Pars_LS) %*% t(X1) + y_1
    Residual_bot_LS[,i] <- (y[-1] - y_fitted_LS) %>% as.numeric()
    
    ##--Forecasts from least squares
    const_LS <- Pars_LS[1]
    alpha_LS <- Pars_LS[2:5]
    beta_LS <- Pars_LS[6:36]
    
    Base_forecasts_bot_LS[i] <- const_LS + y[t] + alpha_LS[1]*(y[t]-y[t-1]) + 
      alpha_LS[2]*(y[t-1] - y[t-2]) + alpha_LS[3]*(y[t-2] - y[t-3]) + 
      alpha_LS[4]*(y[t-3] - y[t-4]) + beta_LS %*% (B[(t),] - B[(t-1),])
    
    
  } 
  
  

  ##Base forecasts and residuals from all levels
  Base_forecasts_LASSO <- c(Base_forecasts_agg_LASSO, Base_forecasts_bot_LASSO)
  Base_forecasts_LS <- c(Base_forecasts_agg_LS, Base_forecasts_bot_LS)
  
  Residual_all_Lasso <- cbind(Residual_agg_Lasso, Residual_bot_Lasso)
  Residual_all_LS <- cbind(Residual_agg_LS, Residual_bot_LS)
  

  ###--Bias correction for LASSO--###
  
  Bias <- apply(Residual_all_Lasso, 2, mean)
  Base_forecasts_biasAdj_LASSO <- Base_forecasts_LASSO - Bias
  
  #--Adding base forecasts to the DF--#
  
  DF <- DF %>% add_row(Series = c(colnames(Prison_allts)), 
                       `F-method` = rep("Lasso", n), 
                       `R-method` = "Base", 
                       Forecasts = Base_forecasts_LASSO,
                       Actual = Test, #[1,]
                       Replication = rep(j, n)) 
  
  DF <- DF %>% add_row(Series = c(colnames(Prison_allts)), 
                       `F-method` = rep("Lasso", n), 
                       `R-method` = "Base_bias.adjust", 
                       Forecasts = Base_forecasts_biasAdj_LASSO,
                       Actual = Test, 
                       Replication = rep(j, n)) 
  
  DF <- DF %>% add_row(Series = c(colnames(Prison_allts)), 
                       `F-method` = rep("Least Squares", n), 
                       `R-method` = "Base", 
                       Forecasts = Base_forecasts_LS,
                       Actual = Test, 
                       Replication = rep(j, n)) 
  
  ###--Reconciliation--###
  
  #--Calculating G matrices--#
  
  # Bottom-up G
  
  Null.ma <- matrix(0,m,(n-m))
  BU_G <- cbind(Null.ma, diag(1,m,m))
  
  # OLS G
  
  OLS_G <- solve(t(S) %*% S) %*% t(S)
  
  # WLS_LASSO G
  
  SamCov_LASSO <- cov(Residual_all_Lasso)
  SamCov_WLS_LASSO <- diag(diag(SamCov_LASSO), n, n)
  Inv_SamCov_WLS_LASSO <- solve(SamCov_WLS_LASSO)
  
  WLS_G_Lasso <- solve(t(S) %*% Inv_SamCov_WLS_LASSO %*% S) %*% t(S) %*% Inv_SamCov_WLS_LASSO
  
  # WLS_LS G
  SamCov_LS <- cov(Residual_all_LS)
  SamCov_WLS_LS <- diag(diag(SamCov_LS), n, n)
  Inv_SamCov_WLS_LS <- solve(SamCov_WLS_LS)

  WLS_G_LS <- solve(t(S) %*% Inv_SamCov_WLS_LS %*% S) %*% t(S) %*% Inv_SamCov_WLS_LS
  
  # MinT(Shrink)_LASSO G
  
  targ <- lowerD(Residual_all_Lasso)
  shrink <- shrink.estim(Residual_all_Lasso,targ)
  Shr.cov_Lasso <- shrink[[1]]
  Inv_Shr.cov_Lasso <- solve(Shr.cov_Lasso)
  
  MinT.shr_G_Lasso <- solve(t(S) %*% Inv_Shr.cov_Lasso %*% S) %*% t(S) %*% Inv_Shr.cov_Lasso
  
  # MinT(Shrink)_LS G
  
  targ <- lowerD(Residual_all_LS)
  shrink <- shrink.estim(Residual_all_LS,targ)
  Shr.cov_LS <- shrink[[1]]
  Inv_Shr.cov_LS <- solve(Shr.cov_LS)
  
  MinT.shr_G_LS <- solve(t(S) %*% Inv_Shr.cov_LS %*% S) %*% t(S) %*% Inv_Shr.cov_LS
  
  #--Reconciling base forecasts--#
  
  #Follows From LASSO
  Recon_LASSO_BU <- S %*% BU_G %*% Base_forecasts_biasAdj_LASSO
  Recon_LASSO_OLS <- S %*% OLS_G %*% Base_forecasts_biasAdj_LASSO
  Recon_LASSO_WLS <- S %*% WLS_G_Lasso %*% Base_forecasts_biasAdj_LASSO
  Recon_LASSO_MinT <- S %*% MinT.shr_G_Lasso %*% Base_forecasts_biasAdj_LASSO
  
  #Follows From Least squars
  Recon_LS_BU <- S %*% BU_G %*% Base_forecasts_LS
  Recon_LS_OLS <- S %*% OLS_G %*% Base_forecasts_LS
  Recon_LS_WLS <- S %*% WLS_G_LS %*% Base_forecasts_LS
  Recon_LS_MinT <- S %*% MinT.shr_G_LS %*% Base_forecasts_LS
  
  
  #--Adding the reconcilied forecasts from LASSO to the DF--#
  
  Fltr <- DF %>% dplyr::filter(`F-method`=="Lasso", `R-method`=="Base_bias.adjust", `Replication`==j) %>% 
    dplyr::select(-Forecasts, -`R-method`)
  
  cbind(Fltr, Forecasts = as.vector(Recon_LASSO_BU), "R-method" = "Bottom-up") -> Df_BU
  Df_BU[names(DF)] -> Df_BU
  DF <- rbind(DF, Df_BU)
  
  cbind(Fltr, Forecasts = as.vector(Recon_LASSO_OLS), "R-method" = "OLS") -> Df_OLS
  Df_OLS[names(DF)] -> Df_OLS
  DF <- rbind(DF, Df_OLS)
  
  cbind(Fltr, Forecasts = as.vector(Recon_LASSO_WLS), "R-method" = "WLS") -> Df_WLS
  Df_WLS[names(DF)] -> Df_WLS
  DF <- rbind(DF, Df_WLS)
  
  cbind(Fltr, Forecasts = as.vector(Recon_LASSO_MinT), "R-method" = "MinT(Shrink)") -> Df_MShr
  Df_MShr[names(DF)] -> Df_MShr
  DF <- rbind(DF, Df_MShr)
  
  
  
  #--Adding the reconcilied forecasts from Least Squares to the DF--#
  
  Fltr <- DF %>% dplyr::filter(`F-method`=="Least Squares", `R-method`=="Base", `Replication`==j) %>% 
    dplyr::select(-Forecasts, -`R-method`)
  
  cbind(Fltr, Forecasts = as.vector(Recon_LS_BU), "R-method" = "Bottom-up") -> Df_BU
  Df_BU[names(DF)] -> Df_BU
  DF <- rbind(DF, Df_BU)
  
  cbind(Fltr, Forecasts = as.vector(Recon_LS_OLS), "R-method" = "OLS") -> Df_OLS
  Df_OLS[names(DF)] -> Df_OLS
  DF <- rbind(DF, Df_OLS)
  
  cbind(Fltr, Forecasts = as.vector(Recon_LS_WLS), "R-method" = "WLS") -> Df_WLS
  Df_WLS[names(DF)] -> Df_WLS
  DF <- rbind(DF, Df_WLS)
  
  cbind(Fltr, Forecasts = as.vector(Recon_LS_MinT), "R-method" = "MinT(Shrink)") -> Df_MShr
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
  filter(`F-method`=="Lasso", `R-method`%in% c("Base_bias.adjust", "OLS")) %>% 
  ggplot(aes(x = Replication, y = MSE, color = `R-method`)) + 
  geom_line()

DF %>% 
  group_by(`F-method`, `R-method`, Replication) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  filter(`F-method`=="Lasso", `R-method`%in% c("Base_bias.adjust", "MinT(Shrink)")) %>% 
  ggplot(aes(x = Replication, y = MSE, color = `R-method`)) + 
  geom_line()


DF %>% 
  group_by(`F-method`, `R-method`, Replication) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  filter(`F-method`=="Least Squares", `R-method`%in% c("Base", "WLS")) %>% 
  ggplot(aes(x = Replication, y = MSE, color = `R-method`)) + 
  geom_line()

