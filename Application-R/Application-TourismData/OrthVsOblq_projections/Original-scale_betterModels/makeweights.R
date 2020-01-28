library(tidyverse)
library(hts)

#Constructs weights used for weighted loss functions

rm(list=ls())
OvernightTrips_Region <- read_csv("OvernightTrips_2017.csv")[,-(1:3)]

## Replacing the odd value in `Adelaide Hills` with the average of its adjecent numbers ##
HierarchyTrips <- suppressMessages(hts(OvernightTrips_Region, list(7, c(6,5,4,4,3,3,2), 
                                                                  c(2,2,1,4,4,1,3,1,3,6,7,3,4,3,2,3,
                                                                    3,4,2,3,1,1,1,2,2,3,4))))
S <- smatrix(HierarchyTrips)

weights<-tibble(Series=unlist(HierarchyTrips$labels),Weight=rowSums(S))

write_csv(weights,'Results/weights.csv')


#Constructs weights using regional expenditure

RegExpend <- read_csv("StateTourismExp.csv",skip = 5,col_names = TRUE)[,-(1:3)]

TotRegExp <- RegExpend %>% colSums()
TotTrips <- OvernightTrips_Region %>% colSums() 

btm_weights <- TotRegExp/TotTrips

AllWeights <- S %*% btm_weights

