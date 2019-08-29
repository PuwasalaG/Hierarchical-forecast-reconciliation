
library(tidyverse)



OvernightTrips_OriginalScaleMisspecified_Fc_1_50 <- read.csv("DF_OriginalScaleMisspecified_1-50.csv")[,-1]
OvernightTrips_OriginalScaleMisspecified_Fc_51_100 <- read.csv("DF_OriginalScaleMisspecified_51-100.csv")[,-1]
OvernightTrips_OriginalScaleMisspecified_Fc_101_140 <- read.csv("DF_OriginalScaleMisspecified_101-140.csv")[,-1]

rbind(OvernightTrips_OriginalScaleMisspecified_Fc_1_50, 
      OvernightTrips_OriginalScaleMisspecified_Fc_51_100, 
      OvernightTrips_OriginalScaleMisspecified_Fc_101_140) %>% 
  as_tibble() -> OvernightTrips_OriginalScaleMisspecified_Fc
write.csv(x=OvernightTrips_OriginalScaleMisspecified_Fc, file = "OvernightTrips_OriginalScaleMisspecified_Fc.csv")


OvernightTrips_OriginalScaleMisspecified_Fc %>% 
  mutate(SquaredE = (Overnight_Trips - Overnight_Trips_Fc)^2) -> OvernightTrips_OriginalScaleMisspecified_Fc

OvernightTrips_OriginalScaleMisspecified_Fc %>% 
  group_by(`R.method`, Fc_horizon) %>% 
  summarise(MSE = mean(SquaredE)) %>% 
  spread(key = Fc_horizon, value = MSE)


