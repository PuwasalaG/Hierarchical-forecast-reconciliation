
library(tidyverse)




# OvernightTrips_OriginalScale_Fc_1_50 <- read.csv("DF_OriginalScale_1-50.csv")[,-1]
# OvernightTrips_OriginalScale_Fc_51_100 <- read.csv("DF_OriginalScale_51-100.csv")[,-1]
# OvernightTrips_OriginalScale_Fc_101_140 <- read.csv("DF_OriginalScale_101-140.csv")[,-1]
# 
# rbind(OvernightTrips_OriginalScale_Fc_1_50, OvernightTrips_OriginalScale_Fc_51_100, 
#       OvernightTrips_OriginalScale_Fc_101_140) %>% 
#   as_tibble() -> OvernightTrips_OriginalScale_Fc
# write.csv(x=OvernightTrips_OriginalScale_Fc, file = "OvernightTrips_OriginalScale_Fc.csv")

OvernightTrips_OriginalScale_Fc <- read.csv("DF_OriginalScale_all.csv")[,-1]
OvernightTrips_OriginalScale_Fc %>% 
  as_tibble() -> OvernightTrips_OriginalScale_Fc
write.csv(x=OvernightTrips_OriginalScale_Fc, file = "OvernightTrips_OriginalScale_Fc.csv")

OvernightTrips_OriginalScale_Fc %>% 
  mutate(SquaredE = (Overnight_Trips - Overnight_Trips_Fc)^2) -> OvernightTrips_OriginalScale_Fc

# OvernightTrips_OriginalScale_Fc %>% 
#   group_by(`R.method`, Fc_horizon) %>% 
#   summarise(MSE = mean(SquaredE)) %>% 
#   spread(key = Fc_horizon, value = MSE)
# 
# 
# OvernightTrips_OriginalScale_Fc <- read.csv("OvernightTrips_OriginalScale_Fc.csv")[,-1]

OvernightTrips_OriginalScale_Fc %>%
  mutate(SquaredE = (Overnight_Trips - Overnight_Trips_Fc)^2,
         WeightedSquaredE = Weight*(Overnight_Trips - Overnight_Trips_Fc)^2,
         Fc_horizon = recode(Fc_horizon, "1" = "h=1", "2" = "h=2",
                             "3" = "h=3", "4" = "h=4", "5" = "h=5", "6" = "h=6")) %>%
  group_by(`R.method`, Fc_horizon, Replication) %>%
  summarise(TSE = sum(SquaredE),WSE = sum(WeightedSquaredE)) -> OvernightTrips_OriginalScale_MSE


# OvernightTrips_OriginalScale_MSE %>%
#   filter(`R.method`%in% c("Base", "OLS", "MinT(Shrink)", "WLS")) %>%
#   spread(key = `R.method`, value = MSE) %>%
#   mutate("Base-OLS" = Base - OLS,
#          "Base-MinT" = Base - `MinT(Shrink)`,
#          "Base-WLS" = Base - WLS) %>%
#   filter(Replication == 10) 
#   
  
# OvernightTrips_OriginalScale_Fc %>% 
#   filter(Fc_horizon == 1, Replication == 15) %>% 
#   summarise(Diff = )
  
