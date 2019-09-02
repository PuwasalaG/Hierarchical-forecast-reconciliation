
library(tidyverse)



DF_LogTrans_1 <- read.csv("DF_LogTrans_WL180_1-42.csv")[,-1]
DF_LogTrans_2 <- read.csv("DF_LogTrans_WL180_43-50.csv")[,-1]
DF_LogTrans_3 <- read.csv("DF_LogTrans_WL180_51-60.csv")[,-1]

rbind(DF_LogTrans_1, DF_LogTrans_2, DF_LogTrans_3) %>% 
  as.data.frame() -> DF_LogTrans_WL180
write.csv(x=DF_LogTrans_WL180, file = "DF_LogTrans_WL180.csv")


DF_LogTrans_WL180 %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF_LogTrans_WL180

# DF_LogTrans_1_50 %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF_LogTrans_1_50
# DF_LogTrans_51_100 %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF_LogTrans_51_100
# DF_LogTrans_101_140 %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF_LogTrans_101_140
# 
# DF_LogTrans_101_140 %>% filter(Replication != 130) %>% 
#   group_by(`F.method`, `R.method`, Forecast_Horizon) %>% 
#   summarise(MSE = mean(SquaredE)) %>% 
#   spread(key = Forecast_Horizon, value = MSE)


DF_LogTrans_WL180 %>% filter(Replication != 103) %>% 
  group_by(`F.method`, `R.method`, Forecast_Horizon) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  filter(Forecast_Horizon == 1) %>% 
  ungroup() %>% 
  dplyr::select(-Forecast_Horizon) %>% 
  spread(key = F.method, value = MSE)


