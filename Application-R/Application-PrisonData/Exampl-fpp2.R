library(fpp2)
library(tidyverse)
library(hts)

prison.gts <- gts(prison/1e3, characters = c(3,1,9),
                  gnames = c("State", "Gender", "Legal",
                             "State*Gender", "State*Legal",
                             "Gender*Legal"))

prison.gts %>% aggts(level=0:3) %>% autoplot()
Prison_allts <- allts(prison.gts)

#BU forecasts for prison data
Fc1 <- forecast(prison.gts, method="bu", fmethod="arima")


S <- smatrix(prison.gts)

#Bottom up G
m <- ncol(prison)
n <- ncol(Prison_allts)

Null.ma <- matrix(0,m,(n-m))
BU_G <- cbind(Null.ma, diag(1,m,m))

Base_forecasts <- matrix(NA, nrow = 8, ncol = n)

for(i in 1:n) {
  fit_training <- auto.arima(Prison_allts[,i])
  Base_forecasts[,i] <- forecast(fit_training, h=8)$mean
}

Recon_BU <- S %*% BU_G %*% t(Base_forecasts)


