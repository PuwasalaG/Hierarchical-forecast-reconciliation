## This file contains the analysis of Tourism data using Hierarchical forecasting methods. 
## Data form a hierarchy with 3 levels - 8-states, -Zones, -Regions. 
## Length of data expands from Jan-1998 to Dec-2017

# Here we fit univariate ARIMA models for the log transformed data
# The back transformed data will be biased and we bias adjust to get 
# unbiased point forecasts.

#We use two methods for bias correction
# M1 - Bias correction via formulation
# M2 - Bias correction via residual mean adjustment

library(tidyverse)
library(fpp2)
library(Matrix)
library(hts)
library(readr)
library(tibble)

# Import bottom level data
#Importing data

OvernightTrips_Region <- read_csv("OvernightTrips_2017.csv")[,-(1:3)]

## Replacing the odd value in `Adelaide Hills` with the average of its adjecent numbers ##

OvernightTrips_Region %>% 
  dplyr::select(`Adelaide Hills`) %>% 
  filter(`Adelaide Hills` > 80) %>% 
  as.numeric() -> a

which(OvernightTrips_Region$`Adelaide Hills`==a) -> x

OvernightTrips_Region %>% 
  dplyr::select(`Adelaide Hills`) %>% 
  filter(row_number() %in% c(60+12,60-12)) %>% 
  summarise(mean(`Adelaide Hills`)) %>% 
  as.numeric() -> b

OvernightTrips_Region_new <- OvernightTrips_Region %>% 
  mutate(`Adelaide Hills` = case_when(`Adelaide Hills`%% a == 0 ~ b, 
                                      TRUE ~ as.double(`Adelaide Hills`)))

#generating the hierarchy
#Hierarchy<-suppressMessages(hts(Bottom_level, list(7, c(14,21,12,12,5,5,7))))
Hierarchy <- suppressMessages(hts(OvernightTrips_Region_new, list(7, c(6,5,4,4,3,3,2), 
                                                                  c(2,2,1,4,4,1,3,1,3,6,7,3,4,3,2,3,
                                                                    3,4,2,3,1,1,1,2,2,3,4))))
AllTS_new <- allts(Hierarchy) %>%
  as_tibble()

#Generating the summing matrix
S <- smatrix(Hierarchy)

n <- ncol(AllTS_new)
l1 <- 1
l2 <- sum(7)
l3 <- sum(6,5,4,4,3,3,2)
m <- ncol(OvernightTrips_Region_new)
H <- 6
L <- 100 #Size of the training sample 
C <- nrow(AllTS_new) - L


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

Start <- Sys.time()

for (j in 1:25) {#C=140
  
  Train <- AllTS_new[j:(L+j-1),]
  Test <- AllTS_new[(L+j):nrow(AllTS_new),]
  
  Names <- colnames(AllTS_new)
  
  Base_forecasts_biased <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  Base_forecasts_unbiased_M1 <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  Base_forecasts_unbiased_M2 <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  
  Residuals_Bias <- list(H1 = matrix(NA, nrow = nrow(Train), ncol = n),
                         H2 = matrix(NA, nrow = nrow(Train), ncol = n), 
                         H3 = matrix(NA, nrow = nrow(Train), ncol = n),
                         H4 = matrix(NA, nrow = nrow(Train), ncol = n),
                         H5 = matrix(NA, nrow = nrow(Train), ncol = n), 
                         H6 = matrix(NA, nrow = nrow(Train), ncol = n))
  
  Residuals_Unbiased_M1 <- matrix(NA, nrow = nrow(Train), ncol = n)
  Residuals_Unbiased_M2 <- list()
  
  Start_fit <- Sys.time()
  
  for (i in 1:n) {
    
    TS <- Train[,i] %>% ts(frequency = 12)
    TS_adj <- TS+1 # adding 1 since some series has zero values which is problamatic when the BoxCox lambda is nearly zero
    
    
    fit_bias <- auto.arima(TS_adj, lambda = "auto", biasadj = FALSE)
    fit_unbias <- auto.arima(TS_adj, lambda = "auto", biasadj = TRUE)
    
    #--Biased and Biased adjusted_M1 forecasts--#
    
    Base_forecasts_biased[,i] <- forecast(fit_bias, h=min(H, nrow(Test)), 
                                          biasadj = FALSE)$mean - 1 #removing the 1 we added initially
    Base_forecasts_unbiased_M1[,i] <- forecast(fit_unbias, h=min(H, nrow(Test)), 
                                               biasadj = TRUE)$mean - 1 #removing the 1 we added initially

    
    
    #--Biased and Biased adjusted_M1 Residuals--#
    
    
    for (h in 1:min(H, nrow(Test))) {
      
      Fitted <- fitted(fit_bias, h=h) - 1
      Residuals_Bias[[h]][,i] <- as.numeric(TS - Fitted)
      
    }
    
    Fitted_biasAdj <- fitted(fit_unbias, h=1) - 1
    Residuals_Unbiased_M1[,i] <- as.vector(TS - Fitted_biasAdj)
    
    
  }
  
  End_fit <- Sys.time()
  
  
  #--Bias correction using method 2--#
  
  Bias <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  
  for (h in 1:min(H, nrow(Test))) {
    
    Bias[h, ] <- apply(Residuals_Bias[[h]], 2, mean, na.rm = TRUE)
    
    Bias_mat <- matrix(rep(Bias[h,], nrow(Train)), nrow = nrow(Train), ncol = n, byrow = T)
    Residuals_Unbiased_M2[[h]] <- Residuals_Bias[[h]] - Bias_mat
  }
  
  Base_forecasts_unbiased_M2 <- Base_forecasts_biased - Bias
  
  
  #--Adding base forecasts to the DF--#
  
  
  if(j < C){
    Test_1 <- Test[1:min(H, nrow(Test)),]%>% as.matrix()
  } else {
    Test_1 <- Test
  }
  
  DF <- DF %>% add_row(Series = rep(Names, each = min(H, nrow(Test_1))), 
                       `F-method` = rep("Bias", n*min(H, nrow(Test_1))), 
                       `R-method` = "Base", 
                       Forecast_Horizon = rep(1:min(H, nrow(Test_1)), n),
                       Forecasts = as.numeric(Base_forecasts_biased),
                       Actual = as.numeric(Test_1), 
                       Replication = rep(j, n*min(H, nrow(Test_1)))) 
  
  DF <- DF %>% add_row(Series = rep(Names, each = min(H, nrow(Test_1))), 
                       `F-method` = rep("Unbiased_M1", n*min(H, nrow(Test_1))), 
                       `R-method` = "Base", 
                       Forecast_Horizon = rep(1:min(H, nrow(Test_1)), n),
                       Forecasts = as.numeric(Base_forecasts_unbiased_M1),
                       Actual = as.numeric(Test_1), 
                       Replication = rep(j, n*min(H, nrow(Test_1)))) 
  
  DF <- DF %>% add_row(Series = rep(Names, each = min(H, nrow(Test_1))), 
                       `F-method` = rep("Unbiased_M2", n*min(H, nrow(Test_1))), 
                       `R-method` = "Base", 
                       Forecast_Horizon = rep(1:min(H, nrow(Test_1)), n),
                       Forecasts = as.numeric(Base_forecasts_unbiased_M2),
                       Actual = as.numeric(Test_1), 
                       Replication = rep(j, n*min(H, nrow(Test_1)))) 
  
  
  ############################
  #--Calculating G matrices--#
  ############################
  
  #-Bottom-up G-#
  
  Null.ma <- matrix(0,m,(n-m))
  BU_G <- cbind(Null.ma, diag(1,m,m))
  
  #-OLS G-#
  
  OLS_G <- solve(t(S) %*% S) %*% t(S)
  
  
  ##--Using biased residuals--##
  
  # Only for forecast horizon h=1 is calculated
  
  #-MinT(Shrink)_biased G-# 
  
  targ_bias <- lowerD(Residuals_Bias[[1]])
  shrink_bias <- shrink.estim(Residuals_Bias[[1]], targ_bias)
  Shr.cov_bias <- shrink_bias[[1]]
  Inv_Shr.cov_bias <- solve(Shr.cov_bias)
  
  MinT.shr_G_bias <- solve(t(S) %*% Inv_Shr.cov_bias %*% S) %*% t(S) %*% 
    Inv_Shr.cov_bias
  
  
  #-WLS_bias G-#
  
  Inv_WLS_bias <- diag(1/diag(Shr.cov_bias), n, n)
  
  WLS_G_bias <- solve(t(S) %*% Inv_WLS_bias %*% S) %*% t(S) %*% 
    Inv_WLS_bias
  
  
  
  ##--Using Unbiased method 1 residuals--##
  
  
  # Only for forecast horizon h=1 is calculated
  
  #-MinT(Shrink)_unbiased_M1 G-# 
  
  targ_unbiased_M1 <- lowerD(Residuals_Unbiased_M1)
  shrink_unbiased_M1 <- shrink.estim(Residuals_Unbiased_M1, 
                                     targ_unbiased_M1)
  Shr.cov_unbiased_M1 <- shrink_unbiased_M1[[1]]
  Inv_Shr.cov_unbiased_M1 <- solve(Shr.cov_unbiased_M1)
  
  MinT.shr_G_unbiased_M1 <- solve(t(S) %*% Inv_Shr.cov_unbiased_M1 %*% S) %*% t(S) %*% 
    Inv_Shr.cov_unbiased_M1
  
  
  #-WLS_bias G-#
  
  Inv_WLS_unbiased_M1 <- diag(1/diag(Shr.cov_unbiased_M1), n, n)
  
  WLS_G_unbiased_M1 <- solve(t(S) %*% Inv_WLS_unbiased_M1 %*% S) %*% t(S) %*% 
    Inv_WLS_unbiased_M1
  
  
  
  ##--Using Unbiased method 2 residuals--##
  
  
  # Only for forecast horizon h=1 is calculated
  
  #-MinT(Shrink)_unbiased_M2 G-# 
  
  targ_unbiased_M2 <- lowerD(Residuals_Unbiased_M2[[1]])
  shrink_unbiased_M2 <- shrink.estim(Residuals_Unbiased_M2[[1]], 
                                     targ_unbiased_M2)
  Shr.cov_unbiased_M2 <- shrink_unbiased_M2[[1]]
  Inv_Shr.cov_unbiased_M2 <- solve(Shr.cov_unbiased_M2)
  
  MinT.shr_G_unbiased_M2 <- solve(t(S) %*% Inv_Shr.cov_unbiased_M2 %*% S) %*% t(S) %*% 
    Inv_Shr.cov_unbiased_M2
  
  
  #-WLS_bias G-#
  
  Inv_WLS_unbiased_M2 <- diag(1/diag(Shr.cov_unbiased_M2), n, n)
  
  WLS_G_unbiased_M2 <- solve(t(S) %*% Inv_WLS_unbiased_M2 %*% S) %*% t(S) %*% 
    Inv_WLS_unbiased_M2
  
  
  ####################
  #--Reconciliation--#
  ####################
  
  #--Reconciling bias base forecasts--#
  
  Recon_Bias_BU <- t(S %*% BU_G %*% t(Base_forecasts_biased))
  Recon_Bias_OLS <- t(S %*% OLS_G %*% t(Base_forecasts_biased))
  Recon_Bias_WLS <- t(S %*% WLS_G_bias %*% t(Base_forecasts_biased))
  Recon_Bias_MinT <- t(S %*% MinT.shr_G_bias %*% t(Base_forecasts_biased))
  
  #Follows From bias adjusted forecasts using Method 1
  Recon_Unbias_M1_BU <- t(S %*% BU_G %*% t(Base_forecasts_unbiased_M1))
  Recon_Unbias_M1_OLS <- t(S %*% OLS_G %*% t(Base_forecasts_unbiased_M1))
  Recon_Unbias_M1_WLS <- t(S %*% WLS_G_unbiased_M1 %*% t(Base_forecasts_unbiased_M1))
  Recon_Unbias_M1_MinT <- t(S %*% MinT.shr_G_unbiased_M1 %*% t(Base_forecasts_unbiased_M1))
  
  #Follows From bias adjusted forecasts using Method 2
  Recon_Unbias_M2_BU <- t(S %*% BU_G %*% t(Base_forecasts_unbiased_M2))
  Recon_Unbias_M2_OLS <- t(S %*% OLS_G %*% t(Base_forecasts_unbiased_M2))
  Recon_Unbias_M2_WLS <- t(S %*% WLS_G_unbiased_M2 %*% t(Base_forecasts_unbiased_M2))
  Recon_Unbias_M2_MinT <- t(S %*% MinT.shr_G_unbiased_M2 %*% t(Base_forecasts_unbiased_M2))
  
  
  #--Adding the reconcilied forecasts from biased base forecasts to the DF--#
  
  Fltr <- DF %>% dplyr::filter(`F-method`=="Bias", `R-method`=="Base", `Replication`==j) %>% 
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
  
  
  #--Adding the reconcilied forecasts from unbiased_M1 base forecasts to the DF--#
  
  Fltr <- DF %>% dplyr::filter(`F-method`=="Unbiased_M1", `R-method`=="Base", `Replication`==j) %>% 
    dplyr::select(-Forecasts, -`R-method`)
  
  cbind(Fltr, Forecasts = as.vector(Recon_Unbias_M1_BU), "R-method" = "Bottom-up") -> Df_BU
  Df_BU[names(DF)] -> Df_BU
  DF <- rbind(DF, Df_BU)
  
  cbind(Fltr, Forecasts = as.vector(Recon_Unbias_M1_OLS), "R-method" = "OLS") -> Df_OLS
  Df_OLS[names(DF)] -> Df_OLS
  DF <- rbind(DF, Df_OLS)
  
  cbind(Fltr, Forecasts = as.vector(Recon_Unbias_M1_WLS), "R-method" = "WLS") -> Df_WLS
  Df_WLS[names(DF)] -> Df_WLS
  DF <- rbind(DF, Df_WLS)
  
  cbind(Fltr, Forecasts = as.vector(Recon_Unbias_M1_MinT), "R-method" = "MinT(Shrink)") -> Df_MShr
  Df_MShr[names(DF)] -> Df_MShr
  DF <- rbind(DF, Df_MShr)
  
  
  #--Adding the reconcilied forecasts from unbiased_M2 base forecasts to the DF--#
  
  Fltr <- DF %>% dplyr::filter(`F-method`=="Unbiased_M2", `R-method`=="Base", `Replication`==j) %>% 
    dplyr::select(-Forecasts, -`R-method`)
  
  cbind(Fltr, Forecasts = as.vector(Recon_Unbias_M2_BU), "R-method" = "Bottom-up") -> Df_BU
  Df_BU[names(DF)] -> Df_BU
  DF <- rbind(DF, Df_BU)
  
  cbind(Fltr, Forecasts = as.vector(Recon_Unbias_M2_OLS), "R-method" = "OLS") -> Df_OLS
  Df_OLS[names(DF)] -> Df_OLS
  DF <- rbind(DF, Df_OLS)
  
  cbind(Fltr, Forecasts = as.vector(Recon_Unbias_M2_WLS), "R-method" = "WLS") -> Df_WLS
  Df_WLS[names(DF)] -> Df_WLS
  DF <- rbind(DF, Df_WLS)
  
  cbind(Fltr, Forecasts = as.vector(Recon_Unbias_M2_MinT), "R-method" = "MinT(Shrink)") -> Df_MShr
  Df_MShr[names(DF)] -> Df_MShr
  DF <- rbind(DF, Df_MShr)
  
  
}

End <- Sys.time()

write.csv(x=DF, file = "Results/DF_BoxCoxTrans_1-25.csv")
save.image("Results/TourismData_BoxCoxTransBiasCorrection_1-25.RData")

DF %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF

DF %>% 
  group_by(`F-method`, `R-method`, Forecast_Horizon) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  spread(key = Forecast_Horizon, value = MSE)

