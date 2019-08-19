
library(tidyverse)



DF_LogTrans_1_50 <- read.csv("DF_LogTrans_1-50.csv")[,-1]
DF_LogTrans_51_100 <- read.csv("DF_LogTrans_51-100.csv")[,-1]
DF_LogTrans_101_140 <- read.csv("DF_LogTrans_101-140.csv")[,-1]

rbind(DF_LogTrans_1_50, DF_LogTrans_51_100, DF_LogTrans_101_140) %>% 
  as.data.frame() -> DF_LogTrans
write.csv(x=DF_LogTrans, file = "DF_LogTrans.csv")


DF_LogTrans %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF_LogTrans

DF_LogTrans %>% 
  group_by(`F.method`, `R.method`, Forecast_Horizon) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  spread(key = Forecast_Horizon, value = MSE)

