
library(tidyverse)



DF_BoxCoxTrans_WL100_1_50 <- read.csv("DF_BoxCoxTrans_WL100_1-50.csv")[,-1] %>% select(-SquaredE)
DF_BoxCoxTrans_WL100_51_100 <- read.csv("DF_BoxCoxTrans_WL100_51-100.csv")[,-1] %>% select(-SquaredE)
DF_BoxCoxTrans_WL100_101_140 <- read.csv("DF_BoxCoxTrans_WL100_101-140.csv")[,-1]

rbind(DF_BoxCoxTrans_WL100_1_50, DF_BoxCoxTrans_WL100_51_100, DF_BoxCoxTrans_WL100_101_140) %>% 
  as.data.frame() -> DF_BoxCoxTrans_WL100
write.csv(x=DF_BoxCoxTrans_WL100, file = "DF_BoxCoxTrans_WL100.csv")


DF_BoxCoxTrans_WL100 %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF_BoxCoxTrans_WL100

DF_BoxCoxTrans_WL100 %>% 
  group_by(`F.method`, `R.method`, Forecast_Horizon) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  spread(key = Forecast_Horizon, value = MSE)


