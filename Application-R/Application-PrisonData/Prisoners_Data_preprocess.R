# Prisoners: Parallel - DT

# Packages
library(tidyverse)
library(magrittr)
library(fpp2)
library(gridExtra)
library(zoo)
library(Matrix)
library(hts)
library(thief)
library(parallel)
library(foreach)
library(doParallel)
library(data.table)
library(readxl)
library(ggsci)


# # Working directory
# setwd("~/Dropbox/RA work/Project 3 - Prisoners Cross-temp")

#### Data ####
#--- Data cleaning ---#
# dfa1 <- read.csv("data_a.csv", strip.white = TRUE)

dfa1 <- read_csv("data_a.csv",
                 col_types = cols(date = col_date(format = "%Y/%m/%d"))) %>%
  as.data.table()

dfa2 <- read_excel("Data_Delivery_Mar17_to_Dec18.xls", 
                   sheet = "Table 1",
                   col_types = c("numeric", "numeric", "numeric", "numeric", 
                                 "date", "numeric"),
                   skip = 2) %>%
  data.table()

setnames(dfa2, old = colnames(dfa2), new = c("state", "sex", "legal", "indigenous", "date", "count"))

dfa1$date %>% class()
dfa2$date %>% class()

dfa1[, "date" := as.Date(`date`)]
dfa2[, "date" := as.Date(`date`)]

rbind(dfa1, dfa2) -> dfa

dfa$indigenous[dfa$indigenous =="3"] <- "2"
dfa$indigenous <- as.character(dfa$indigenous)

dfa$legal[dfa$legal =="2"] <- "1"
dfa$legal <- as.character(dfa$legal)

dfa$legal[dfa$legal =="3"] <- "2"
dfa$legal <- as.character(dfa$legal)

dfa$legal[dfa$legal =="4"] <- "2"
dfa$legal <- as.character(dfa$legal)

dfa$count <- as.numeric(dfa$count)

dfa <- aggregate(count ~ state + sex + legal + indigenous + date, data = dfa, FUN = sum)

head(dfa)
tail(dfa)

dfa_new <- dfa %>% 
  group_by(state, sex, legal, date) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  as.data.frame()

## Getting the descriptions clearer in the attributes

dfa_new$state[dfa_new$state =="1"] <- "NSW"
dfa_new$state[dfa_new$state =="2"] <- "VIC"
dfa_new$state[dfa_new$state =="3"] <- "QLD"
dfa_new$state[dfa_new$state =="4"] <- "SA "
dfa_new$state[dfa_new$state =="5"] <- "WA "
dfa_new$state[dfa_new$state =="6"] <- "TAS"
dfa_new$state[dfa_new$state =="7"] <- "NT "
dfa_new$state[dfa_new$state =="8"] <- "ACT"

dfa_new$sex[dfa_new$sex =="1"] <- "M"
dfa_new$sex[dfa_new$sex =="2"] <- "F"

dfa_new$legal[dfa_new$legal =="1"] <- "Remanded "
dfa_new$legal[dfa_new$legal =="2"] <- "Sentenced"

dfa_new$t <- as.Date(dfa_new$date, format = "%Y/%m/%d")
dfa_new$count <- as.numeric(dfa_new$count)
dfa_new$quarter <- as.Date(cut(dfa_new$t, breaks = "quarter"))
dfa_new$year <- as.Date(cut(dfa_new$t, breaks="year"))

dfa_new_w <- dfa_new %>%
  reshape(idvar = c("state", "sex", "legal"), 
                     timevar = "date", direction = "wide")

dfa_new_w$pathString <- paste(dfa_new_w$state,#length=1
                              dfa_new_w$sex, #length=1
                              dfa_new_w$legal, sep="")



dfa_new_w %>% colnames() -> coln
coln[str_detect(coln, "^count")] -> .old
.old %>% str_extract(., "..........$") -> .new
setnames(dfa_new_w, old = .old, new = .new)

myvars <- c("pathString", .new)
newdata <- dfa_new_w[myvars]

# Transpose the data
newdata2 <- t(newdata)

# Rename the columns and remove the first row
colnames(newdata2) = newdata2[1,]
newdata2 = newdata2[-1,]

newdata2[newdata2 == 'n/a'] = NA
newdata2 = apply(newdata2, 2, as.numeric)

Prison_botts <- ts(newdata2, start=c(2005,1), end=c(2018,4), frequency=4)
write.csv(Prison_botts, file = "Prison.bottom.TS.csv")


# Prison.gts <- gts(Prison_botts/1e3, characters = c(3,1,9),
#                   gnames = c("State", "Gender", "Legal",
#                              "State*Gender", "State*Legal",
#                              "Gender*Legal"))
# 
# 
# Prison_allts <- allts(Prison.gts)
# m <- ncol(Prison_botts)
# n <- ncol(Prison_allts)
# 
# smatrix(data.y) -> Sc





