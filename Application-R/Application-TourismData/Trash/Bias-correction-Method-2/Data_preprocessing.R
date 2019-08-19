## Change Geelong to Geelong and the Bellarine


library(tidyverse)
Data_OvernightTrips <- read_csv("Tourism_Australia_FullDataSet_upto_2017.csv", skip = 9)
Data_Nights <- read_csv("VN_FullDataSet Edited by George2017.csv", skip = 6)[,-1] %>%
  rename(Year = `Calendar year`)

Data_OvernightTrips <- Data_OvernightTrips[,names(Data_Nights)]

Data_OvernightTrips %>% 
  mutate(Wimmera = Wimmera+Mallee) %>% 
  select(-Mallee) %>% 
  rename(Wimmera_Mallee = Wimmera) -> Data_OvernightTrips

write.csv(Data_OvernightTrips, "OvernightTrips_2017.csv")



