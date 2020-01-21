library(tidyverse)
library(tsutils)
library(tidyr)
library(Matrix)
library(readr)
library(tibble)
library(kableExtra)

DF_LogTrans_WL100 <- read.csv("DF_LogTrans_WL100.csv")[,-1]

DF_LogTrans_WL100 <- DF_LogTrans_WL100 %>% 
  mutate(F.method=recode(F.method, `Bias` = 'Biased')) 
# so that we don't confuse Biased forecasts with ME/bias

DF_LogTrans_WL100 %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF_LogTrans_WL100
DF_LogTrans_WL100 %>% mutate(ME = `Actual` - `Forecasts`) -> DF_LogTrans_WL100


# # ME calculations
# DF_LogTrans_WL100 %>%
#   filter(Forecast_Horizon == 1) %>% 
#   group_by(`F.method`, `R.method`) %>% 
#   summarise(ME = sum(ME)/1e3) %>% 
#   arrange(ME)

# Nemenyi for ME
DF_LogTrans_WL100 %>% 
  filter(Forecast_Horizon == 1) %>% 
  group_by(Replication, F.method, R.method) %>% 
  summarise(ME = sum(ME)/1e3) %>%
  mutate(method=paste(F.method,R.method, sep='_')) %>% 
  ungroup() %>% 
  select(Replication, ME ,method) %>% 
  spread(method,ME) %>% as.matrix() -> m

  m <- m[,-1] 
  class(m) <- "numeric"

  nemenyi(m,conf.level=0.95,plottype="matrix")

# dm.test(e1,e2, alternative = "two.sided", h = 1, power = 2)

# colSums(m) %>% as.matrix() -> ME
# ME <-  ME[order(ME[,1]),, drop=F]  %>% as.matrix()
# ME # ME


# # Verifying MSE calculations 
# DF_LogTrans_WL100 %>%
#   filter(Forecast_Horizon == 1) %>%
#   group_by(`F.method`, `R.method`) %>%
#   summarise(MSE = round(mean(SquaredE)/1e3, digits = 6)) %>%
#   arrange(MSE)

# Nemenyi for MSE
DF_LogTrans_WL100 %>%
  filter(Forecast_Horizon == 1) %>% 
  group_by(Replication, F.method, R.method) %>% 
  summarise(MSE = mean(SquaredE)/1e3) %>%
  mutate(method=paste(F.method,R.method, sep='_')) %>% 
  ungroup() %>% 
  select(Replication, MSE ,method) %>% 
  spread(method,MSE) %>% as.matrix() -> m

  m <- m[,-1] 
  class(m) <- "numeric"

  nemenyi(m,conf.level=0.90,plottype="matrix")

  # colMeans(m) %>% as.matrix() -> MSE
  # MSE <-  MSE[order(MSE[,1]),, drop=F]  %>% as.matrix()
  # MSE # MSE


  DF_BoxCoxTrans_WL100 <- read.csv("DF_BoxCoxTrans_WL100.csv")[,-1]
  
  DF_BoxCoxTrans_WL100 <- DF_BoxCoxTrans_WL100 %>% 
    mutate(F.method=recode(F.method, `Bias` = 'Biased')) 
  # so that we don't confuse Biased forecasts with ME/bias
  
  DF_BoxCoxTrans_WL100 %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF_BoxCoxTrans_WL100
  DF_BoxCoxTrans_WL100 %>% mutate(ME = `Actual` - `Forecasts`) -> DF_BoxCoxTrans_WL100
  
  
  # # ME calculations
  # DF_BoxCoxTrans_WL100 %>%
  #   filter(Forecast_Horizon == 1) %>%
  #   group_by(`F.method`, `R.method`) %>%
  #   summarise(ME = sum(ME)/1e3) %>%
  #   arrange(ME)

  # Nemenyi for ME
  DF_BoxCoxTrans_WL100 %>% 
    filter(Forecast_Horizon == 1) %>% 
    group_by(Replication, F.method, R.method) %>% 
    summarise(ME = sum(ME)/1e3) %>%
    mutate(method=paste(F.method,R.method, sep='_')) %>% 
    ungroup() %>% 
    select(Replication, ME ,method) %>% 
    spread(method,ME) %>% as.matrix() -> m
  
  m <- m[,-1] 
  class(m) <- "numeric"
  
  nemenyi(m,conf.level=0.90,plottype="matrix")
  
  # dm.test(e1,e2, alternative = "two.sided", h = 1, power = 2)
  
  # colSums(m) %>% as.matrix() -> ME
  # ME <-  ME[order(ME[,1]),, drop=F]  %>% as.matrix()
  # ME # ME
  
  
  # # Verifying MSE calculations 
  # DF_BoxCoxTrans_WL100 %>%
  #   filter(Forecast_Horizon == 1) %>%
  #   group_by(`F.method`, `R.method`) %>%
  #   summarise(MSE = round(mean(SquaredE)/1e3, digits = 6)) %>%
  #   arrange(MSE)
  
  # Nemenyi for MSE
  DF_BoxCoxTrans_WL100 %>%
    filter(Forecast_Horizon == 1) %>% 
    group_by(Replication, F.method, R.method) %>% 
    summarise(MSE = mean(SquaredE)/1e3) %>%
    mutate(method=paste(F.method,R.method, sep='_')) %>% 
    ungroup() %>% 
    select(Replication, MSE ,method) %>% 
    spread(method,MSE) %>% as.matrix() -> m
  
  m <- m[,-1] 
  class(m) <- "numeric"
  
  nemenyi(m,conf.level=0.90,plottype="matrix")
  
  # colMeans(m) %>% as.matrix() -> MSE
  # MSE <-  MSE[order(MSE[,1]),, drop=F]  %>% as.matrix()
  # MSE # MSE


