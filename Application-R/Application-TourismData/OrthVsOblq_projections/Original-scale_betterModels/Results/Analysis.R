
library(tidyverse)



OvernightTrips_OriginalScale_Fc_1_50 <- read.csv("DF_OriginalScale_1-50.csv")[,-1]
OvernightTrips_OriginalScale_Fc_51_100 <- read.csv("DF_OriginalScale_51-100.csv")[,-1]
OvernightTrips_OriginalScale_Fc_101_140 <- read.csv("DF_OriginalScale_101-140.csv")[,-1]

rbind(OvernightTrips_OriginalScale_Fc_1_50, OvernightTrips_OriginalScale_Fc_51_100, 
      OvernightTrips_OriginalScale_Fc_101_140) %>% 
  as_tibble() -> OvernightTrips_OriginalScale_Fc
write.csv(x=OvernightTrips_OriginalScale_Fc, file = "OvernightTrips_OriginalScale_Fc.csv")


OvernightTrips_OriginalScale_Fc %>% 
  mutate(SquaredE = (Overnight_Trips - Overnight_Trips_Fc)^2) -> OvernightTrips_OriginalScale_Fc

OvernightTrips_OriginalScale_Fc %>% 
  group_by(`R.method`, Fc_horizon) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  spread(key = Fc_horizon, value = MSE)


