
library(tidyverse)



DF_BoxCoxTrans_1_50 <- read.csv("DF_BoxCoxTrans_set2_1-50.csv")[,-1] 
DF_BoxCoxTrans_51_100 <- read.csv("DF_BoxCoxTrans_set2_51-100.csv")[,-1]
DF_BoxCoxTrans_101_140 <- read.csv("DF_BoxCoxTrans_set2_101-140.csv")[,-1]

rbind(DF_BoxCoxTrans_1_50, DF_BoxCoxTrans_51_100, DF_BoxCoxTrans_101_140) %>% 
  as.data.frame() -> DF_BoxCoxTrans
write.csv(x=DF_BoxCoxTrans, file = "DF_BoxCoxTrans_set2.csv")


DF_BoxCoxTrans %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF_BoxCoxTrans

DF_BoxCoxTrans %>% 
  group_by(`F.method`, `R.method`, Forecast_Horizon) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  spread(key = Forecast_Horizon, value = MSE)


DF_BoxCoxTrans %>% 
  group_by(`F.method`, `R.method`, Forecast_Horizon) %>% 
  filter(Replication != 115) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  spread(key = Forecast_Horizon, value = MSE)
