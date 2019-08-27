
library(tidyverse)



DF_BoxCoxTrans_1_25 <- read.csv("DF_BoxCoxTrans_1-25.csv")[,-1] %>% select(-SquaredE)
DF_BoxCoxTrans_26_50 <- read.csv("DF_BoxCoxTrans_26-50.csv")[,-1] %>% select(-SquaredE)

DF_BoxCoxTrans_51_75 <- read.csv("DF_BoxCoxTrans_51-75.csv")[,-1] %>% select(-SquaredE)
DF_BoxCoxTrans_76_100 <- read.csv("DF_BoxCoxTrans_76-100.csv")[,-1] %>% select(-SquaredE)

DF_BoxCoxTrans_101_125 <- read.csv("DF_BoxCoxTrans_101-125.csv")[,-1] %>% select(-SquaredE)
DF_BoxCoxTrans_126_140 <- read.csv("DF_BoxCoxTrans_126-140.csv")[,-1]

rbind(DF_BoxCoxTrans_1_25, DF_BoxCoxTrans_26_50, DF_BoxCoxTrans_51_75, 
      DF_BoxCoxTrans_76_100, DF_BoxCoxTrans_101_125, DF_BoxCoxTrans_126_140) %>% 
  as.data.frame() -> DF_BoxCoxTrans
write.csv(x=DF_BoxCoxTrans, file = "DF_BoxCoxTrans.csv")


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
