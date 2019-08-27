library(tidyverse)
library(gridExtra)

OvernightTrips_Region <- read_csv("OvernightTrips_2017.csv")[,-(1:3)]

## Replacing the odd value in `Adelaide Hills` with the average of its adjecent numbers ##

OvernightTrips_Region %>% 
  dplyr::select(`Adelaide Hills`) %>% 
  filter(`Adelaide Hills` > 80) %>% 
  as.numeric() -> a

which(OvernightTrips_Region$`Adelaide Hills`==a) -> x

OvernightTrips_Region %>% 
  dplyr::select(`Adelaide Hills`) %>% 
  filter(row_number() %in% c(60+12,60-12)) %>% 
  summarise(mean(`Adelaide Hills`)) %>% 
  as.numeric() -> b

OvernightTrips_Region_new <- OvernightTrips_Region %>% 
  mutate(`Adelaide Hills` = case_when(`Adelaide Hills`%% a == 0 ~ b, 
                                      TRUE ~ as.double(`Adelaide Hills`)))

#generating the hierarchy
#Hierarchy<-suppressMessages(hts(Bottom_level, list(7, c(14,21,12,12,5,5,7))))
Hierarchy <- suppressMessages(hts(OvernightTrips_Region_new, 
                                  list(7, c(6,5,4,4,3,3,2), c(2,2,1,4,4,1,3,1,3,6,7,3,4,3,2,3,3,4,2,3,1,1,1,2,2,3,4))))

AllTS <- allts(Hierarchy) %>%
  as_tibble()

AllTS_new <- AllTS + 1 # Add 1 since some series has zero observations and this will affect taking log transformations

#Total Series

AllTS %>% log() %>% 
  dplyr::select("Total") %>% 
  ts(start = c(1998, 1), frequency = 12) %>% 
  autoplot() + ylab("log(Overnight trips)") + xlab("Time") + 
  theme(legend.position="bottom") -> TSPlot_total

AllTS %>% log() %>% 
  dplyr::select("Total") %>% 
  ts(start = c(1998, 1), frequency = 12) %>% 
  ggsubseriesplot() + ylab("log(Overnight trips)") + 
  theme(legend.position="bottom") +
  ggtitle("Subseries plot") -> subseries_total

AllTS %>% log() %>% 
  dplyr::select("Total") %>% 
  ts(start = c(1998, 1), frequency = 12) %>% 
  ggseasonplot() + ylab("log(Overnight trips)") + 
  theme(legend.position="bottom") +
  ggtitle("Seasonal plot") -> seasonalplot_total

grid.arrange( arrangeGrob(subseries_total),
              arrangeGrob(seasonalplot_total),
              ncol=2) -> subseries_seasonal_total

grid.arrange( arrangeGrob(TSPlot_total, top = "Total Overnight trips"), 
              arrangeGrob(subseries_seasonal_total), ncol = 1, heights = c(4,4))




#States

AllTS_new %>% log() %>% 
  dplyr::select("A", "B", "C", "D", "E", "F", "G" ) %>% 
  rename("NSW" = "A", "Victoria" = "B", "Queensland" = "C", 
         "South Australia" = "D", "Western Australia" = "E", "Tasmania" = "F",
         "Northern Territory" = "G") %>% 
  ts(start = c(1998, 1), frequency = 12) %>% 
  autoplot() + ylab("log(Overnight trips)") + xlab("Time") + 
  scale_y_log10() +
  ggtitle("States") + theme(legend.position="bottom") -> Plot_Sates

#Zones

# "AA", "AB", "AC", "AD", "AE", "AF", "BA", "BB", "BC", "BD", "BE",
# "CA", "CB", "CC", "CD", "DA", "DB", "DC", "DD", "EA", "EB","EC", 
# "FA", "FB", "FC", "GA", "GB"

AllTS_new %>% log() %>% 
  dplyr::select("AA", "AF", "BB", "BE",
                "CA", "CD", "DC", "DD", "EB","EC", 
                "FA", "FB", "GA", "GB") %>% 
  rename("Metro NSW" = "AA", "ACT" = "AF", "West Coast VIC" = "BB",
         "North East VIC" = "BE", "Metro QLD" = "CA", "Inland QLD" = "CD",
         "Inland SA" = "DC", "West Coast SA" = "DD", "North WA" = "EB",
         "South WA" = "EC", "South TAS" = "FA", "North East TAS" = "FB",
         "North Coast NT" = "GA", "Central NT" = "GB") %>% 
  ts(start = c(1998, 1), frequency = 12) %>% 
  autoplot() + ylab("log(Overnight trips)") + xlab("Time") + 
  ggtitle("Zones") + theme(legend.position="bottom") -> Plot_Zones


AllTS_new %>% log() %>% 
  dplyr::select("Central Coast", "Hunter",
                "Riverina", "Canberra", "Peninsula", 
                "Melbourne", "Phillip Island") %>% 
  ts(start = c(1998, 1), frequency = 12) %>% 
  autoplot() + ylab("log(Overnight trips)") + xlab("Time") + 
  ggtitle("Regions") + theme(legend.position="bottom") -> Plot_regions_1

AllTS_new %>% log() %>% 
  dplyr::select("Bendigo Loddon",
                "Gold Coast", "Tropical North Queensland", "Adelaide Hills", 
                "Experience Perth", "Darwin") %>% 
  rename("Tropical Nth QLD" = "Tropical North Queensland", "Perth" = "Experience Perth") %>% 
  ts(start = c(1998, 1), frequency = 12) %>% 
  autoplot() + ylab("log(Overnight trips)") + xlab("Time") + 
  ggtitle("Regions") + theme(legend.position="bottom") -> Plot_regions_2

grid.arrange(arrangeGrob(Plot_regions_1), arrangeGrob(Plot_regions_2), ncol = 2) -> Plot_regions

grid.arrange(arrangeGrob(Plot_Sates, Plot_Zones, Plot_regions ))




###############################

###############################

# Series related to State - A
AllTS %>% 
  dplyr::select("A") %>% 
  ts(start = c(1998, 1), frequency = 12) %>% 
  autoplot() + ylab("Overnight trips") + xlab("Time") + 
  ggtitle("State-A") + theme(legend.position="bottom") -> Plot_A

AllTS %>% 
  dplyr::select("AA", "AB", "AC", "AD", "AE", "AF") %>% 
  ts(start = c(1998, 1), frequency = 12) %>%  
  as.tsibble() %>% rename("Zones" = "key") %>% 
  ggplot(aes(x = index, y = value, color = Zones)) + 
  geom_line()  + ylab("Overnight trips") + xlab("Time") + 
  ggtitle("Zones in State A") + theme(legend.position="bottom") -> Plot_A_Zone

AllTS %>% 
  dplyr::select("AAA", "AAB", "ABA", "ABB", "ACA", "ADA", "ADB", "ADC", "ADD", "AEA", "AEB",
                "AEC", "AED", "AFA") %>% 
  ts(start = c(1998, 1), frequency = 12) %>%  
  as.tsibble() %>% rename("Regions" = "key") %>% 
  ggplot(aes(x = index, y = value, color = Regions)) + 
  geom_line()  + ylab("Overnight trips") + xlab("Time") + 
  ggtitle("Regions in State A") + theme(legend.position = "bottom") -> Plot_A_Region

grid.arrange(arrangeGrob(Plot_A, Plot_A_Zone, Plot_A_Region))

# Series related to State - B
AllTS %>% 
  dplyr::select("B") %>% 
  ts(start = c(1998, 1), frequency = 12) %>% 
  autoplot() + ylab("Overnight trips") + xlab("Time") + 
  ggtitle("State-B") + theme(legend.position="bottom") -> Plot_B

AllTS %>% 
  dplyr::select("BA", "BB", "BC", "BD", "BE") %>% 
  ts(start = c(1998, 1), frequency = 12) %>%  
  as.tsibble() %>% rename("Zones" = "key") %>% 
  ggplot(aes(x = index, y = value, color = Zones)) + 
  geom_line()  + ylab("Overnight trips") + xlab("Time") + 
  ggtitle("Zones in State B") + theme(legend.position="bottom") -> Plot_B_Zone

AllTS %>% 
  dplyr::select("BAA", "BAB", "BAC", "BBA", "BCA", "BCB", "BCC", "BDA", "BDB",
                "BDC", "BDD", "BDE", "BDF", "BEA", "BEB", "BEC", "BED", "BEE",
                "BEF", "BEG") %>% 
  ts(start = c(1998, 1), frequency = 12) %>%  
  as.tsibble() %>% rename("Regions" = "key") %>% 
  ggplot(aes(x = index, y = value, color = Regions)) + 
  geom_line()  + ylab("Overnight trips") + xlab("Time") + 
  ggtitle("Regions in State B") + theme(legend.position = "bottom") -> Plot_B_Region

grid.arrange(arrangeGrob(Plot_B, Plot_B_Zone, Plot_B_Region))

# Series related to State - C
AllTS %>% 
  dplyr::select("C") %>% 
  ts(start = c(1998, 1), frequency = 12) %>% 
  autoplot() + ylab("Overnight trips") + xlab("Time") + 
  ggtitle("State-C") + theme(legend.position="bottom") -> Plot_C

AllTS %>% 
  dplyr::select("CA", "CB", "CC", "CD") %>% 
  ts(start = c(1998, 1), frequency = 12) %>%  
  as.tsibble() %>% rename("Zones" = "key") %>% 
  ggplot(aes(x = index, y = value, color = Zones)) + 
  geom_line()  + ylab("Overnight trips") + xlab("Time") + 
  ggtitle("Zones in State C") + theme(legend.position="bottom") -> Plot_C_Zone

AllTS %>% 
  dplyr::select("CAA", "CAB", "CAC", "CBA", "CBB", "CBC", "CBD",
                "CCA", "CCB", "CCC", "CDA", "CDB") %>% 
  ts(start = c(1998, 1), frequency = 12) %>%  
  as.tsibble() %>% rename("Regions" = "key") %>% 
  ggplot(aes(x = index, y = value, color = Regions)) + 
  geom_line()  + ylab("Overnight trips") + xlab("Time") + 
  ggtitle("Regions in State C") + theme(legend.position = "bottom") -> Plot_C_Region

grid.arrange(arrangeGrob(Plot_C, Plot_C_Zone, Plot_C_Region))

# Series related to State - D
AllTS %>% 
  dplyr::select("D") %>% 
  ts(start = c(1998, 1), frequency = 12) %>% 
  autoplot() + ylab("Overnight trips") + xlab("Time") + 
  ggtitle("State-D") + theme(legend.position="bottom") -> Plot_D

AllTS %>% 
  dplyr::select("DA", "DB", "DC", "DD") %>% 
  ts(start = c(1998, 1), frequency = 12) %>%  
  as.tsibble() %>% rename("Zones" = "key") %>% 
  ggplot(aes(x = index, y = value, color = Zones)) + 
  geom_line()  + ylab("Overnight trips") + xlab("Time") + 
  ggtitle("Zones in State D") + theme(legend.position="bottom") -> Plot_D_Zone

AllTS %>% 
  dplyr::select("DAA", "DAB", "DAC", "DBA", "DBB", "DBC", "DCA",
                "DCB", "DCC", "DCD", "DDA", "DDB") %>% 
  ts(start = c(1998, 1), frequency = 12) %>%  
  as.tsibble() %>% rename("Regions" = "key") %>% 
  ggplot(aes(x = index, y = value, color = Regions)) + 
  geom_line()  + ylab("Overnight trips") + xlab("Time") + 
  ggtitle("Regions in State D") + theme(legend.position = "bottom") -> Plot_D_Region

grid.arrange(arrangeGrob(Plot_D, Plot_D_Zone, Plot_D_Region))
