## This file contains the analysis of Tourism data using Hierarchical forecasting methods.
## Data form a hierarchy with 3 levels - 8-states, -Zones, -Regions.
## Length of data expands from Jan-1998 to Dec-2017

# Here we fit univariate ARIMA models for the BoxCOx transformed data
# The back transformed data will be biased and we bias adjust to get
# unbiased point forecasts.

# We noticed that there is an unusual observation in the Adelaide Hills series. So we
# replace that odd value with the average of adjecent values


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



n <- ncol(AllTS_new)
l1 <- 1
l2 <- sum(7)
l3 <- sum(6,5,4,4,3,3,2)
m <- ncol(OvernightTrips_Region_new)
H <- 12
L <- 200 #Size of the training sample
C <- nrow(AllTS_new) - L


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

for (j in 1:10) {#C = 140
j=10

  Train <- AllTS_new[j:(L+j-1),]
  Test <- AllTS_new[(L+j):nrow(AllTS_new),]

  # if(j==C){
  #   Test <- as.matrix(Test) %>% t()
  # }
  Names <- colnames(AllTS_new)

  Base_forecasts <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  Residuals_Base <- matrix(NA, nrow = nrow(Train), ncol = n)

  Start_fit <- Sys.time()

  for (i in 1:n) {

    TS <- Train[,i] %>% as.ts()
    fit <- auto.arima(TS)

    Fc <- forecast(fit, h=min(H, nrow(Test)))
    Fc_mean <- Fc$mean
    #L_PI <- Fc$lower[,"95%"]
    #sig_h <- (Fc_mean - L_PI)/1.96
    #sig2_h <- fit$sigma2


    # Base_forecasts_biased[,i] <- forecast(fit_bias, lambda = "auto", h=min(H, nrow(Test)), biasadj = FALSE)$mean
    # Base_forecasts_unbiased[,i] <- forecast(fit_unbiased, lambda = "auto", h=min(H, nrow(Test)), biasadj = TRUE)$mean
    #
    # Residuals_BackTransformed[,i] <- residuals(fit_bias, h=1, type = "response")
    # Residuals_BackTransformed_biasadjust[,i] <- residuals(fit_unbiased, h=1, type = "response")

    Base_forecasts[,i] <- Fc_mean
    Residuals_Base[,i] <- as.vector(fit$residuals)
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
                       `F-method` = rep("ARIMA", n*min(H, nrow(Test_1))),
                       `R-method` = "Base",
                       Forecast_Horizon = rep(1:min(H, nrow(Test_1)), n),
                       Forecasts = as.numeric(Base_forecasts),
                       Actual = as.numeric(Test_1),
                       Replication = rep(j, n*min(H, nrow(Test_1))))


  ###--Reconciliation--###

  #--Calculating G matrices--#

  # Bottom-up G

  Null.ma <- matrix(0,m,(n-m))
  BU_G <- cbind(Null.ma, diag(1,m,m))

  # OLS G
  OLS_G <- solve(t(S) %*% S) %*% t(S)

  # MinT(Shrink)_BackTrans G (This G matrix is calculated using back-transformed residuals)
  targ <- diag(diag(var(Residuals_Base)), n, n)
  shrink <- shrink.estim(Residuals_Base, targ)
  Shr.cov <- shrink[[1]]
  Inv_Shr.cov <- solve(Shr.cov)

  MinT.shr_G <- solve(t(S) %*% Inv_Shr.cov %*% S) %*% t(S) %*% Inv_Shr.cov

  # WLS_BackTrans G (This G matrix is calculated using back-transformed residuals)
  Inv_WLS <- diag(1/diag(var(Residuals_Base)), n, n)

  WLS_G <- solve(t(S) %*% Inv_WLS %*% S) %*% t(S) %*% Inv_WLS

  #--Reconciling base forecasts--#
  Recon_BU <- t(S %*% BU_G %*% t(Base_forecasts))
  Recon_OLS <- t(S %*% OLS_G %*% t(Base_forecasts))
  Recon_WLS <- t(S %*% WLS_G %*% t(Base_forecasts))
  Recon_MinT <- t(S %*% MinT.shr_G %*% t(Base_forecasts))

  #--Adding the reconcilied forecasts from biased base forecasts to the DF--#

  Fltr <- DF %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base", `Replication`==j) %>%
    dplyr::select(-Forecasts, -`R-method`)

  cbind(Fltr, Forecasts = as.vector(Recon_BU), "R-method" = "Bottom-up") -> Df_BU
  Df_BU[names(DF)] -> Df_BU
  DF <- rbind(DF, Df_BU)

  cbind(Fltr, Forecasts = as.vector(Recon_OLS), "R-method" = "OLS") -> Df_OLS
  Df_OLS[names(DF)] -> Df_OLS
  DF <- rbind(DF, Df_OLS)

  cbind(Fltr, Forecasts = as.vector(Recon_WLS), "R-method" = "WLS") -> Df_WLS
  Df_WLS[names(DF)] -> Df_WLS
  DF <- rbind(DF, Df_WLS)

  cbind(Fltr, Forecasts = as.vector(Recon_MinT), "R-method" = "MinT(Shrink)") -> Df_MShr
  Df_MShr[names(DF)] -> Df_MShr
  DF <- rbind(DF, Df_MShr)

}

End <- Sys.time()

save.image("TourismData_BoxCoxTransBiasCorrection_UnivARIMA_new.RData")

DF %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF

DF %>%
  group_by(`F-method`, `R-method`, Forecast_Horizon) %>%
  summarise(MSE = mean(SquaredE)) %>%
  spread(key = Forecast_Horizon, value = MSE)

DF %>%
  group_by(`F-method`, `R-method`, Replication, Forecast_Horizon) %>%
  summarise(MSE = mean(SquaredE)) %>%
  filter(`F-method`=="ARIMA", `R-method`%in% c("Base", "OLS","WLS","MinT(Shrink)")) %>%
  ggplot(aes(x = Replication, y = MSE, color = `R-method`)) +
  geom_line() +
  facet_wrap( ~ Forecast_Horizon, scales = "free_y")



## Box-plots for the differences of MSE between OLS, MinT vs the base

DF_MSE <- DF %>%
  group_by(`F-method`, `R-method`, Replication, Forecast_Horizon) %>%
  summarise(MSE = mean(SquaredE))


DF_MSE %>%
  filter(`F-method`=="ARIMA", `R-method`%in% c("Base", "OLS", "MinT(Shrink)","WLS")) %>%
  spread(key = `R-method`, value = MSE) %>%
  mutate("Base-OLS" = Base - OLS,
         "Base-MinT" = Base - `MinT(Shrink)`,
         "Base-WLS" = Base - WLS) %>%
  select(Replication, Forecast_Horizon, `Base-OLS`, `Base-MinT`,`Base-WLS`) %>%
  gather(`Base-OLS`,`Base-MinT`, `Base-WLS`, key = Method, value = MSE) %>%
  ggplot(aes(x = Method, y = MSE)) + geom_boxplot() + facet_wrap(~`Forecast_Horizon`)


DF_MSE %>%
  filter(`F-method`=="ARIMA", `R-method`%in% c("Base", "OLS", "MinT(Shrink)","WLS")) %>%
  spread(key = `R-method`, value = MSE) %>%
  mutate("Base-OLS" = Base - OLS,
         "Base-MinT" = Base - `MinT(Shrink)`,
         "Base-WLS" = Base - WLS) %>%
  select(Replication, Forecast_Horizon, `Base-OLS`, `Base-MinT`,`Base-WLS`) %>%
  filter(Forecast_Horizon==1)


