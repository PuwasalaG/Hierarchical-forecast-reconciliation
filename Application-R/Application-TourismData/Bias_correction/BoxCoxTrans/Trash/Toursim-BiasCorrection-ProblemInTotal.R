library(tidyverse)
library(Matrix)
library(hts)
library(tsibble)
library(fable)




# Bottom level data

OvernightTrips_Region <- read_csv("OvernightTrips_2017.csv")[,-(1:3)]

## Replacing the anomalous observation in `Adelaide Hills` ##

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

Hierarchy <- suppressMessages(hts(OvernightTrips_Region_new, list(7, c(6,5,4,4,3,3,2), 
                                                                  c(2,2,1,4,4,1,3,1,3,6,7,3,4,3,2,3,
                                                                    3,4,2,3,1,1,1,2,2,3,4))))
AllTS <- allts(Hierarchy) %>% 
  ts(start = c(1998, 1), frequency = 12) %>% 
  as_tsibble()

AllTS %>% 
  group_by(key) %>% 
  slice(1:2)


AllTS %>% 
  filter(key == "Total") %>% 
  slice(67:166) %>% 
  auto.arima(lambda = "auto", biasadj = TRUE, stepwise = FALSE, approximation = FALSE)

AllTS %>% 
  filter(key == "Total") %>% 
  slice(67:166) %>% features(value, features = guerrero) %>%
  pull(lambda_guerrero) -> lambda

AllTS %>% 
  filter(key == "Total") %>% 
  slice(67:166) %>% 
  model(ARIMA(box_cox(value, lambda = lambda), stepwise = FALSE, approximation = FALSE)) %>% report()

AllTS %>% 
  filter(key == "Total") %>% 
  slice(67:166) %>% 
  model(ARIMA(box_cox(value, lambda = lambda))) %>% 
          residuals(type = "response", h=1)


AllTS %>% 
  filter(key == "Total") %>% 
  slice(93:192) %>% 
  auto.arima(lambda = "auto", biasadj = TRUE, stepwise = FALSE, approximation = FALSE)
