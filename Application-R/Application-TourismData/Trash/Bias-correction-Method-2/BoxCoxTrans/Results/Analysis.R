
library(tidyverse)



DF_1_50 <- read.csv("DF_1-50.csv")[,-1]
DF_51_100 <- read.csv("DF_51-100.csv")[,-1]
DF_101_140 <- read.csv("DF_101-140.csv")[,-1]

rbind(DF_1_50, DF_51_100, DF_101_140) %>% 
  as.data.frame() -> DF
write.csv(x=DF, file = "DF_BoxCoxTrans_BiasAdj_M2.csv")


DF %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF

DF %>% 
  group_by(`F.method`, `R.method`, Forecast_Horizon) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  spread(key = Forecast_Horizon, value = MSE)

