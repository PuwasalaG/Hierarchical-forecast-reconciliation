library(tidyverse)
library(gridExtra)

load("TourismData_LogTransBiasCorrection_UnivARIMA.RData")

#Total Series
AllTS %>% 
  dplyr::select("Total") %>% 
  ts(start = c(1998, 1), frequency = 12) %>% 
  autoplot() + ylab("Overnight trips") + xlab("Time") + 
  ggtitle("Total") + theme(legend.position="bottom") -> Plot_total

#States

AllTS %>% 
  dplyr::select("A", "B", "C", "D", "E", "F", "G" ) %>% 
  rename("NSW" = "A", "Victoria" = "B", "Queensland" = "C", 
         "South Australia" = "D", "Western Australia" = "E", "Tasmania" = "F",
         "Northern Territory" = "G") %>% 
  ts(start = c(1998, 1), frequency = 12) %>% 
  autoplot() + ylab("Overnight trips") + xlab("Time") + 
  ggtitle("States") + theme(legend.position="bottom") -> Plot_Sates

#Zones

AllTS %>% 
  dplyr::select("AA", "AB", "AC", "AD", "AE", "AF", "BA", "BB", "BC", "BD", "BE",
                "CA", "CB", "CC", "CD", "DA", "DB", "DC", "DD", "EA", "EB","EC", 
                "FA", "FB", "FC", "GA", "GB") %>% 
  ts(start = c(1998, 1), frequency = 12) %>% 
  autoplot() + ylab("Overnight trips") + xlab("Time") + 
  ggtitle("States") + theme(legend.position="bottom")


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
