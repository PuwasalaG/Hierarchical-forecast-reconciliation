## Change Geelong to Geelong and the Bellarine


library(tidyverse)
Data_OvernightTrips <- read_csv("Tourism_Australia_FullDataSet_upto_2017.csv", skip = 9)
Data_Nights <- read_csv("VN_FullDataSet.csv", skip = 6)[,-1] %>%
  rename(Year = `Calendar year`)


Data_OvernightTrips <- Data_OvernightTrips[,names(Data_Nights)]

Data_OvernightTrips %>% 
  mutate(Wimmera = Wimmera+Mallee) %>% 
  dplyr::select(-Mallee) %>% 
  rename(Wimmera_Mallee = Wimmera) -> Data_OvernightTrips

# colnames(Data_OvernightTrips) <- c("Year", "Month returned", "AAA", "AAB",	"ABA", "ABB",	"ACA", "ADA",	"ADB", "ADC",	"ADD", "AEA",	"AEB",
#                            "AEC", "AED",	"AFA", "BAA",	"BAB", "BAC",	"BBA", "BCA",	"BCB", "BCC",	"BDA",
#                            "BDB", "BDC",	"BDD", "BDE",	"BDF", "BEA", "BEB", "BEC",	"BED", "BEE",	"BEF",
#                            "BEG", "CAA", "CAB",	"CAC",	"CBA", "CBB",	"CBC", "CBD",	"CCA", "CCB",
#                            "CCC", "CDA",	"CDB", "DAA",	"DAB", "DAC",	"DBA", "DBB",	"DBC", "DCA",	"DCB", "DCC",
#                            "DCD", "DDA",	"DDB", "EAA",	"EAB", "EAC",	"EBA", "ECA",	"FAA", "FBA",	"FBB", "FCA",
#                            "FCB", "GAA", "GAB", "GAC", "GBA", "GBB",	"GBC", "GBD")


write.csv(Data_OvernightTrips, "OvernightTrips_2017.csv")



