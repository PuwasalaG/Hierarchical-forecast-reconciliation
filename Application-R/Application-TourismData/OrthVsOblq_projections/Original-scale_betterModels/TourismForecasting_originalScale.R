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
library(tsibble)
library(fable)
library(fabletools)

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
AllTS <- allts(Hierarchy) %>%
  as_tibble()

Names <- names(AllTS)
#Generating the summing matrix
S <- smatrix(Hierarchy)

n <- ncol(AllTS)
N <- nrow(AllTS)
l1 <- 1
l2 <- sum(7)
l3 <- sum(6,5,4,4,3,3,2)
m <- ncol(OvernightTrips_Region_new)
H <- 6
L <- 100 #Size of the training sample 
C <- nrow(AllTS) - L

#Compute W for structural scaling

W_struct<-diag(rowSums(S))

AllTS_new <- AllTS %>% 
  ts(start = c(1998,1), frequency = 12) %>% 
  as_tsibble() %>% 
  rename("Series" = key, "Overnight_Trips" = value)



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

#To store Final results of all replications

Final_DF <- tibble("index" = character(),
                   "Series" = factor(),
                   "R-method" = character(),
                   "Fc_horizon" = integer(),
                   "Overnight_Trips_Fc" = double(),
                   "Overnight_Trips" = double(),
                   "Replication" = double())

Start <- Sys.time()

for (j in 1:140) {#C=140
  print(paste('replication',j))
  Train <- AllTS_new %>%
    group_by(Series) %>%
    slice(j:(L+j-1)) %>%
    ungroup() %>%
    mutate(Series = fct_relevel(Series, Names)) %>%
    arrange(Series)

  Test <- AllTS_new %>%
    group_by(Series) %>%
    slice((L+j):N) %>%
    ungroup() %>%
    mutate(Series = fct_relevel(Series, Names)) %>%
    arrange(Series)
  
  Nrow_Test <- Test %>% distinct(index) %>% nrow()

###############################
  #Model fitting using fable#
###############################
  
  Start_fit <- Sys.time()
  
  fit <- Train %>% 
    group_by(Series) %>% 
    model(ARIMA = ARIMA())
  
  Base_Fc <- fit %>% forecast(h = min(H, Nrow_Test))
  Residuals <- fit %>% residuals(type = "response")
  
  Base_forecasts <- Base_Fc %>% 
    mutate(Fc_horizon = rep(1:min(H, Nrow_Test), n)) %>% 
    as_tibble() %>% 
    select(Series, Overnight_Trips, Fc_horizon) %>% 
    spread(key = Series, value = Overnight_Trips) %>%
    select(-Fc_horizon) %>% 
    as.matrix()
  
  Residual_all <- Residuals %>% 
    spread(key = Series, value = .resid) %>% 
    as_tibble %>% select(-index, -.model) %>% 
    as.matrix()
  

  End_fit <- Sys.time()
  

###############################
###############################  
  

  #--Adding base forecasts to the DF--#
  
  
  
  Base_Fc %>% 
    as_tibble() %>% 
    select(index, Series, Overnight_Trips) %>%
    rename("Overnight_Trips_Fc" =  Overnight_Trips) %>% 
    mutate(Fc_horizon = rep(1:min(H, Nrow_Test), n)) %>% 
    mutate("R-method" = rep("Base", min(H, Nrow_Test)*n)) -> DF_Base
  
  Test %>% group_by(Series) %>% slice(1:min(H, Nrow_Test)) -> test
  
  left_join(DF_Base, test) %>% 
    mutate(index = yearmonth(index)) %>% 
    add_column(Replication = j) -> DF
  
  Index <- DF %>% pull(index)

  
  
  # if(j < C){
  #   Test_1 <- Test[1:min(H, Nrow_Test),]%>% as.matrix()
  # } else {
  #   Test_1 <- Test
  # }
  # 

  
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
  
  targ_bias <- lowerD(Residual_all)
  shrink_bias <- shrink.estim(Residual_all, targ_bias)
  Shr.cov_bias <- shrink_bias[[1]]
  Inv_Shr.cov_bias <- solve(Shr.cov_bias)
  
  MinT.shr_G_bias <- solve(t(S) %*% Inv_Shr.cov_bias %*% S) %*% t(S) %*% 
    Inv_Shr.cov_bias
  
  
  #-WLS_bias G-# THIS HAS BEEN CHANGED TO STRUCTURAL SCALING
  InvRt_W<- diag(1/sqrt(diag(W_struct)), n, n)
    
  Inv_WLS_bias <- InvRt_W%*%InvRt_W
  
  WLS_G_bias <- solve(t(S) %*% Inv_WLS_bias %*% S) %*% t(S) %*% 
    Inv_WLS_bias
  
  #G MinT
  
    
    
  GMinT.shr_G_bias <- solve(t(S) %*% InvRt_W %*%Inv_Shr.cov_bias%*% InvRt_W %*% S) %*% t(S) %*% InvRt_W%*% 
    Inv_Shr.cov_bias%*% InvRt_W
  

  ####################
  #--Reconciliation--#
  ####################
  
  Recon_BU <- t(S %*% BU_G %*% t(Base_forecasts))
  Recon_OLS <- t(S %*% OLS_G %*% t(Base_forecasts))
  Recon_WLS <- t(S %*% WLS_G_bias %*% t(Base_forecasts))
  Recon_MinT <- t(S %*% MinT.shr_G_bias %*% t(Base_forecasts))
  Recon_GMinT <- t(S %*% GMinT.shr_G_bias %*% t(Base_forecasts))
  
##########################################################  
  #--Adding the reconcilied forecasts to the Final_DF--#
##########################################################
  
  
  ##--Adding Bottom-up Forecasts--##
  
  colnames(Recon_BU) <- Names

  Recon_BU %>% 
    as_tibble() %>% 
    add_column(index = rep(yearmonth(Index[1]) + 0:(min(H, Nrow_Test)-1))) %>% 
    gather(-index, key = Series, value = "Overnight_Trips_Fc") %>% 
    mutate("Fc_horizon" = rep(1:min(H, Nrow_Test), n),
           "R-method" = rep("Bottom-up", min(H, Nrow_Test)*n), 
           Series = as_factor(Series)) -> Recon_BU
  
  left_join(Recon_BU, test) %>% 
    add_column(Replication = j) -> DF_Recon_BU
  
  rbind(DF, DF_Recon_BU) -> DF
  
  ##--Adding OLS Forecasts--##
  
  colnames(Recon_OLS) <- Names
  
  Recon_OLS %>% 
    as_tibble() %>% 
    add_column(index = rep(yearmonth(Index[1]) + 0:(min(H, Nrow_Test)-1))) %>% 
    gather(-index, key = Series, value = "Overnight_Trips_Fc") %>% 
    mutate("Fc_horizon" = rep(1:min(H, Nrow_Test), n),
           "R-method" = rep("OLS", min(H, Nrow_Test)*n), 
           Series = as_factor(Series)) -> Recon_OLS
  
  left_join(Recon_OLS, test) %>% 
    add_column(Replication = j) -> DF_Recon_OLS
  
  rbind(DF, DF_Recon_OLS) -> DF
  
  ##--Adding MinT(Shrink) Forecasts--##
  
  colnames(Recon_MinT) <- Names
  
  Recon_MinT %>% 
    as_tibble() %>% 
    add_column(index = rep(yearmonth(Index[1]) + 0:(min(H, Nrow_Test)-1))) %>% 
    gather(-index, key = Series, value = "Overnight_Trips_Fc") %>% 
    mutate("Fc_horizon" = rep(1:min(H, Nrow_Test), n),
           "R-method" = rep("MinT(Shrink)", min(H, Nrow_Test)*n), 
           Series = as_factor(Series)) -> Recon_MinT
  
  left_join(Recon_MinT, test) %>% 
    add_column(Replication = j) -> DF_Recon_MinT
  
  rbind(DF, DF_Recon_MinT) -> DF
  
  
  ##--Adding WLS Forecasts--##
  
  colnames(Recon_WLS) <- Names
  
  Recon_WLS %>% 
    as_tibble() %>% 
    add_column(index = rep(yearmonth(Index[1]) + 0:(min(H, Nrow_Test)-1))) %>% 
    gather(-index, key = Series, value = "Overnight_Trips_Fc") %>% 
    mutate("Fc_horizon" = rep(1:min(H, Nrow_Test), n),
           "R-method" = rep("WLS", min(H, Nrow_Test)*n), 
           Series = as_factor(Series)) -> Recon_WLS
  
  left_join(Recon_WLS, test) %>% 
    add_column(Replication = j) -> DF_Recon_WLS
  
  rbind(DF, DF_Recon_WLS) -> DF
  
  ##--Adding GMinT Forecasts--##
  
  colnames(Recon_GMinT) <- Names
  
  Recon_GMinT %>% 
    as_tibble() %>% 
    add_column(index = rep(yearmonth(Index[1]) + 0:(min(H, Nrow_Test)-1))) %>% 
    gather(-index, key = Series, value = "Overnight_Trips_Fc") %>% 
    mutate("Fc_horizon" = rep(1:min(H, Nrow_Test), n),
           "R-method" = rep("GMinT", min(H, Nrow_Test)*n), 
           Series = as_factor(Series)) -> Recon_GMinT
  
  left_join(Recon_GMinT, test) %>% 
    add_column(Replication = j) -> DF_Recon_GMinT 
  
  rbind(DF, DF_Recon_GMinT) -> DF  
  
  ###################################
      ### ADDING TO FINAL DF ###
  ###################################
  
  rbind(Final_DF, DF) -> Final_DF
  
  
}

End <- Sys.time()

write.csv(x=DF, file = "Results/DF_OriginalScale_all.csv")
save.image("Results/TourismForecasting_originalScale_all.RData")


# DF %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF
# 
# DF %>% 
#   group_by(`F-method`, `R-method`, Forecast_Horizon) %>% 
#   summarise(MSE = mean(SquaredE)) %>% 
#   spread(key = Forecast_Horizon, value = MSE)
# 
