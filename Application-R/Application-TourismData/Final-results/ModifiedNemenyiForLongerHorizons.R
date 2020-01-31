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
DF_LogTrans_WL100 %>% mutate(ME = `Actual` - `Forecasts`)  -> DF_LogTrans_WL100


# # ME calculations
# DF_LogTrans_WL100 %>%
#   filter(Forecast_Horizon == 1) %>% 
#   group_by(`F.method`, `R.method`) %>% 
#   summarise(ME = sum(ME)/1e3) %>% 
#   arrange(ME)

DF_LogTrans_WL100 %>%  head()
DF_LogTrans_WL100 %>% select(F.method) %>% unique()

# Being quick about this and not spending time to generalise

# We drop Unbiased (Method 2 and WLS)
DF_LogTrans_WL100 <- DF_LogTrans_WL100 %>% 
  filter(F.method!="Unbiased_M2", R.method!="WLS") 

# New ME calculations
DF_LogTrans_WL100%>% 
  group_by(F.method, R.method, Series, Forecast_Horizon) %>% 
  summarise(ME = sum(ME)/1e3) %>%
  group_by(F.method,R.method, Forecast_Horizon) %>% 
  summarise(AME=sum(abs(ME))) %>% 
  ungroup() %>% 
  pivot_wider(names_from = c('F.method','R.method'),values_from =AME) %>%
  mutate_at(vars(`Biased_Bottom-up`:Unbiased_M1_OLS),funs(./Biased_Base)) %>% 
  mutate(Biased_Base=1) %>% 
  pivot_longer(cols = -Forecast_Horizon) %>% 
  pivot_wider(names_from = 'Forecast_Horizon')


# Nemenyi for MSE
DF_LogTrans_WL100 %>%
  group_by(F.method, R.method, Forecast_Horizon) %>% 
  summarise(MSE = mean(SquaredE)/1e3) %>% 
  ungroup() %>% 
  pivot_wider(names_from = c('F.method','R.method'),values_from =MSE) %>%
  mutate_at(vars(`Biased_Bottom-up`:Unbiased_M1_OLS),funs(./Biased_Base)) %>% 
  mutate(Biased_Base=1) %>% 
  pivot_longer(cols = -Forecast_Horizon) %>% 
  pivot_wider(names_from = 'Forecast_Horizon')



# Box Cox
DF_BoxCoxTrans_WL100 <- read.csv("DF_BoxCoxTrans_WL100.csv")[,-1]

DF_BoxCoxTrans_WL100 <- DF_BoxCoxTrans_WL100 %>% 
  mutate(F.method=recode(F.method, `Bias` = 'Biased')) 
# so that we don't confuse Biased forecasts with ME/bias

DF_BoxCoxTrans_WL100 %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2) -> DF_BoxCoxTrans_WL100
DF_BoxCoxTrans_WL100 %>% mutate(ME = `Actual` - `Forecasts`) -> DF_BoxCoxTrans_WL100

# We drop Unbiased (Method 2 and WLS)
DF_BoxCoxTrans_WL100 <- DF_BoxCoxTrans_WL100 %>% 
  filter(F.method!="Unbiased_M2", R.method!="WLS") 



# New ME calculations
DF_BoxCoxTrans_WL100%>% 
  group_by(F.method, R.method, Series, Forecast_Horizon) %>% 
  summarise(ME = sum(ME)/1e3) %>%
  group_by(F.method,R.method, Forecast_Horizon) %>% 
  summarise(AME=sum(abs(ME))) %>% 
  ungroup() %>% 
  pivot_wider(names_from = c('F.method','R.method'),values_from =AME) %>%
  mutate_at(vars(`Biased_Bottom-up`:Unbiased_M1_OLS),funs(./Biased_Base)) %>% 
  mutate(Biased_Base=1) %>% 
  pivot_longer(cols = -Forecast_Horizon) %>% 
  pivot_wider(names_from = 'Forecast_Horizon')


# Nemenyi for MSE
DF_BoxCoxTrans_WL100 %>%
  group_by(F.method, R.method, Forecast_Horizon) %>% 
  summarise(MSE = mean(SquaredE)/1e3) %>% 
  ungroup() %>% 
  pivot_wider(names_from = c('F.method','R.method'),values_from =MSE) %>%
  mutate_at(vars(`Biased_Bottom-up`:Unbiased_M1_OLS),funs(./Biased_Base)) %>% 
  mutate(Biased_Base=1) %>% 
  pivot_longer(cols = -Forecast_Horizon) %>% 
  pivot_wider(names_from = 'Forecast_Horizon')


