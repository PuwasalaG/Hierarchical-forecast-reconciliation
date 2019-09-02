
library(tidyverse)



DF_LogTrans_WL100_1_50 <- read.csv("DF_LogTrans_WL100_1-50.csv")[,-1]
DF_LogTrans_WL100_51_100 <- read.csv("DF_LogTrans_WL100_51-100.csv")[,-1]
DF_LogTrans_WL100_101_140 <- read.csv("DF_LogTrans_WL100_101-140.csv")[,-1]

rbind(DF_LogTrans_1_50, DF_LogTrans_51_100, DF_LogTrans_101_140) %>% 
  as.data.frame() -> DF_LogTrans_WL100
write.csv(x=DF_LogTrans_WL100, file = "DF_LogTrans_WL100.csv")


DF_LogTrans_WL100 %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF_LogTrans_WL100

# DF_LogTrans_1_50 %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF_LogTrans_1_50
# DF_LogTrans_51_100 %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF_LogTrans_51_100
# DF_LogTrans_101_140 %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF_LogTrans_101_140
# 
# DF_LogTrans_101_140 %>% filter(Replication != 130) %>% 
#   group_by(`F.method`, `R.method`, Forecast_Horizon) %>% 
#   summarise(MSE = mean(SquaredE)) %>% 
#   spread(key = Forecast_Horizon, value = MSE)


DF_LogTrans_WL100 %>% filter(Replication != 103) %>% 
  group_by(`F.method`, `R.method`, Forecast_Horizon) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  filter(Forecast_Horizon == 1) %>% 
  ungroup() %>% 
  dplyr::select(-Forecast_Horizon) %>% 
  spread(key = F.method, value = MSE)


