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
for (j in 1:C) {#C
  

  Train <- Prison_allts[j:(36+j-1),]
  Test <- Prison_allts[(36+j):56,]
  Bts_tr <- Train[,(n-m+1):n]
  
  Base_forecasts_agg_LASSO <- numeric((n-m))
  Base_forecasts_agg_LS <- numeric((n-m))
  
  Residual_agg_Lasso <- matrix(NA, nrow = nrow(Train)-p, ncol = (n-m))
  Residual_agg_LS <- matrix(NA, nrow = nrow(Train), ncol = (n-m))
  
  Variance_agg_LS <- numeric((n-m))
  
  for (i in 1:(n-m)) {
    
    Y <- data.frame(Train[,i], Bts_tr)
    VARX <- list()
    VARX$k <- 1
    VARX$s <- p
    
    #VARX with LASSO for aggregate levels
    Var_agg_lasso.ft <- constructModel(as.matrix(Y), p = p, struct = "Basic", gran = c(25, 10),
                            verbose = F, VARX = VARX, h=1)
    Var_agg_lasso.Results <- cv.BigVAR(Var_agg_lasso.ft)
    
    Base_forecasts_agg_LASSO[i] <- predict(Var_agg_lasso.Results, 1)
    Residual_agg_Lasso[,i] <- Var_agg_lasso.Results@resids
    
    
    #VARX with LS for aggregate levels
    Var_agg_ls.ft <- VARXFit(as.matrix(Y), p = p, IC = NULL, VARX = VARX)
    Base_forecasts_agg_LS[i] <- PredictVARX(Var_agg_ls.ft)
    Variance_agg_LS[i] <- Var_agg_ls.ft$SigmaU

    
    
  } 
  
  
  #VAR(1) for disaggregate levels with LASSO
  Var_bot_lasso.ft <- constructModel(as.matrix(Bts_tr), p = 1, struct = "Basic", gran = c(25, 10),
                                     verbose = F, h=1)
  
  Var_bot_lasso.Results <- cv.BigVAR(Var_bot_lasso.ft)
  
  Base_forecasts_bot_LASSO <- predict(Var_bot_lasso.Results, 1)
  Residual_bot_Lasso <- Var_bot_lasso.Results@resids
  
  #VAR(1) for disaggregate levels with LS
  Var_bot_ls.ft <- VARXFit(as.matrix(Bts_tr), p = 1, IC = NULL)
  
  Base_forecasts_bot_LS <- PredictVARX(Var_bot_ls.ft)
  Variance_bot_LS <- diag(Var_bot_ls.ft$SigmaU)
  
  
  ##Base forecasts and residuals from all levels
  Base_forecasts_LASSO <- c(Base_forecasts_agg_LASSO, Base_forecasts_bot_LASSO)
  Base_forecasts_LS <- c(Base_forecasts_agg_LS, Base_forecasts_bot_LS)

  Residual_all_Lasso <- cbind(Residual_agg_Lasso, Residual_bot_Lasso)
  #Residual_agg_LS <- matrix(NA, nrow = nrow(Train), ncol = (n-m))
  Variance_all_LS <- c(Variance_agg_LS, Variance_bot_LS)
  Var_mat_LS <- diag(Variance_all_LS, nrow = n, ncol = n)
  
  
  ###--Bias correction for LASSO--###
  
  Bias <- apply(Residual_all_Lasso, 2, mean)
  Base_forecasts_biasAdj_LASSO <- Base_forecasts_LASSO - Bias
  
  #--Adding base forecasts to the DF--#
  
  DF <- DF %>% add_row(Series = c(colnames(Prison_allts)), 
                       `F-method` = rep("Lasso", n), 
                       `R-method` = "Base", 
                       Forecasts = Base_forecasts_LASSO,
                       Actual = Test[1,], 
                       Replication = rep(j, n)) 
  
  DF <- DF %>% add_row(Series = c(colnames(Prison_allts)), 
                       `F-method` = rep("Lasso", n), 
                       `R-method` = "Base_bias.adjust", 
                       Forecasts = Base_forecasts_biasAdj_LASSO,
                       Actual = Test[1,], 
                       Replication = rep(j, n)) 
  
  DF <- DF %>% add_row(Series = c(colnames(Prison_allts)), 
                       `F-method` = rep("Least Squares", n), 
                       `R-method` = "Base", 
                       Forecasts = Base_forecasts_LS,
                       Actual = Test[1,], 
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
  
  Inv_SamCov_WLS_LS <- solve(Var_mat_LS)
  
  WLS_G_LS <- solve(t(S) %*% Inv_SamCov_WLS_LS %*% S) %*% t(S) %*% Inv_SamCov_WLS_LS
  
  # MinT(Shrink)_LASSO G
  
  targ <- lowerD(Residual_all_Lasso)
  shrink <- shrink.estim(Residual_all_Lasso,targ)
  Shr.cov_Lasso <- shrink[[1]]
  Inv_Shr.cov_Lasso <- solve(Shr.cov_Lasso)
  
  MinT.shr_G_Lasso <- solve(t(S) %*% Inv_Shr.cov_Lasso %*% S) %*% t(S) %*% Inv_Shr.cov_Lasso
    
  
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

