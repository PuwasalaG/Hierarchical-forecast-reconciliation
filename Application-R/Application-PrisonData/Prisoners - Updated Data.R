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

if (getDoParWorkers() == 1){
  # Registering parellel backend
  cl <- makeCluster(detectCores()-1)
  registerDoParallel(cl)
  
  print(paste("Registered doParallel using", getDoParName(), "with", getDoParWorkers(), "workers."))
  
}

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

## Getting the descriptions clearer in the attributes
dfa$state[dfa$state =="1"] <- "NSW"
dfa$state[dfa$state =="2"] <- "VIC"
dfa$state[dfa$state =="3"] <- "QLD"
dfa$state[dfa$state =="4"] <- "SA"
dfa$state[dfa$state =="5"] <- "WA"
dfa$state[dfa$state =="6"] <- "TAS"
dfa$state[dfa$state =="7"] <- "NT"
dfa$state[dfa$state =="8"] <- "ACT"

dfa$sex[dfa$sex =="1"] <- "Male"
dfa$sex[dfa$sex =="2"] <- "Female"

dfa$indigenous[dfa$indigenous =="1"] <- "ATSI"
dfa$indigenous[dfa$indigenous =="2"] <- "Non-ATSI"

dfa$legal[dfa$legal =="1"] <- "Remanded"
dfa$legal[dfa$legal =="2"] <- "Sentenced"

dfa$t <- as.Date(dfa$date, format = "%Y/%m/%d")
dfa$count <- as.numeric(dfa$count)
dfa$quarter <- as.Date(cut(dfa$t, breaks = "quarter"))
dfa$year <- as.Date(cut(dfa$t, breaks="year"))

dfa_w <- reshape(dfa, idvar = c("state", "sex", "legal", "indigenous"), timevar = "date", direction = "wide")

dfa_w$pathString <- paste(dfa_w$state,#length=1
                          dfa_w$sex, #length=1
                          dfa_w$legal, #length=1
                          dfa_w$indigenous,#length=1
                          sep="")

# Rename the columns

# setnames(dfa_w, old=c("count.2005/03/01","count.2005/06/01","count.2005/09/01","count.2005/12/01",
#                       "count.2006/03/01", "count.2006/06/01", "count.2006/09/01", "count.2006/12/01",
#                       "count.2007/03/01", "count.2007/06/01", "count.2007/09/01", "count.2007/12/01",
#                       "count.2008/03/01", "count.2008/06/01", "count.2008/09/01", "count.2008/12/01",
#                       "count.2009/03/01", "count.2009/06/01", "count.2009/09/01", "count.2009/12/01",
#                       "count.2010/03/01", "count.2010/06/01", "count.2010/09/01", "count.2010/12/01",
#                       "count.2011/03/01", "count.2011/06/01", "count.2011/09/01", "count.2011/12/01",
#                       "count.2012/03/01","count.2012/06/01","count.2012/09/01","count.2012/12/01",
#                       "count.2013/03/01", "count.2013/06/01", "count.2013/09/01", "count.2013/12/01",
#                       "count.2014/03/01", "count.2014/06/01", "count.2014/09/01", "count.2014/12/01",
#                       "count.2015/03/01", "count.2015/06/01", "count.2015/09/01", "count.2015/12/01",
#                       "count.2016/03/01","count.2016/06/01","count.2016/09/01","count.2016/12/01"),
#          new=c("2005/03/01", "2005/06/01", "2005/09/01", "2005/12/01", "2006/03/01", "2006/06/01",
#                "2006/09/01", "2006/12/01", "2007/03/01", "2007/06/01", "2007/09/01", "2007/12/01",
#                "2008/03/01", "2008/06/01", "2008/09/01", "2008/12/01", "2009/03/01", "2009/06/01",
#                "2009/09/01", "2009/12/01", "2010/03/01", "2010/06/01", "2010/09/01", "2010/12/01",
#                "2011/03/01", "2011/06/01", "2011/09/01", "2011/12/01", "2012/03/01", "2012/06/01",
#                "2012/09/01", "2012/12/01", "2013/03/01", "2013/06/01", "2013/09/01", "2013/12/01",
#                "2014/03/01", "2014/06/01", "2014/09/01", "2014/12/01", "2015/03/01", "2015/06/01",
#                "2015/09/01", "2015/12/01", "2016/03/01", "2016/06/01", "2016/09/01", "2016/12/01"))
# myvars <- c("pathString","2005/03/01", "2005/06/01", "2005/09/01", "2005/12/01", "2006/03/01",
#             "2006/06/01", "2006/09/01", "2006/12/01", "2007/03/01", "2007/06/01", "2007/09/01",
#             "2007/12/01", "2008/03/01", "2008/06/01", "2008/09/01", "2008/12/01", "2009/03/01",
#             "2009/06/01", "2009/09/01", "2009/12/01", "2010/03/01", "2010/06/01", "2010/09/01",
#             "2010/12/01", "2011/03/01", "2011/06/01", "2011/09/01", "2011/12/01", "2012/03/01",
#             "2012/06/01", "2012/09/01", "2012/12/01", "2013/03/01", "2013/06/01", "2013/09/01",
#             "2013/12/01", "2014/03/01", "2014/06/01", "2014/09/01", "2014/12/01", "2015/03/01", 
#             "2015/06/01", "2015/09/01", "2015/12/01", "2016/03/01", "2016/06/01", "2016/09/01", 
#             "2016/12/01")


dfa_w %>% colnames() -> coln
coln[str_detect(coln, "^count")] -> .old
.old %>% str_extract(., "..........$") -> .new
setnames(dfa_w, old = .old, new = .new)

myvars <- c("pathString", .new)
newdata <- dfa_w[myvars]

# Transpose the data
newdata2 <- t(newdata)

# Rename the columns and remove the first row
colnames(newdata2) = newdata2[1,]
newdata2 = newdata2[-1,]

newdata2[newdata2 == 'n/a'] = NA
newdata2 = apply(newdata2, 2, as.numeric)

bts <- ts(newdata2, start=c(2005,1), end=c(2018,4), frequency=4)

#nchar(newdata$pathString) #4
#ncol(bts)

#y.gts <- gts(bts, characters = c(1,1,1,1))
#allts(y.gts)[,2]

# 8 states, 2 sex, 2 gender, 2 indigenous
state <- rep(c("NSW", "VIC", "QLD", "SA", "WA", "NT", "ACT", "TAS"), 64/8)
ind <- rep(rep(c("I","NI"),each=16),2)
sex <- rep(rep(c("m","f"),each=8),4)
leg <- rep(c("r","s"),each=64/2)

state_sex <- as.character(interaction(state,sex,sep=""))
state_leg <- as.character(interaction(state,leg, sep=""))
state_ind <- as.character(interaction(state,ind, sep=""))
sex_leg <- as.character(interaction(sex,leg, sep=""))
sex_ind <- as.character(interaction(sex,ind,sep=""))
ind_leg <- as.character(interaction(ind,leg, sep=""))

state_sex_leg <- as.character(interaction(state,sex,leg,sep=""))
state_sex_ind <- as.character(interaction(state,sex,ind,sep=""))
state_leg_ind <- as.character(interaction(state,leg,ind,sep=""))

sex_leg_ind <- as.character(interaction(sex,leg,ind,sep=""))


#state_sex_ind <- as.character(interaction(state,sex,ind, sep=""))

gc <-rbind(state,sex,ind,leg, state_sex, state_ind, state_leg, sex_ind,
           sex_leg, ind_leg, state_sex_leg, state_sex_ind, state_leg_ind,
           sex_leg_ind)

data.y <- gts(bts,groups=gc)
smatrix(data.y) -> Sc

#### Plots of the Original data ####
aggts(data.y) %>%
  as.data.table() -> Plot.dt

Plot.dt[, "Time" := as.Date(time(aggts(data.y)[,"Total"]))]

Plot.dt %>%
  melt(., id.vars = "Time", variable.name = "Variable", value.name = "Value") -> Plot.dt.g

theme_x <- function(.font = font){
  
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, margin = margin(r=1.5, l=0.1, unit="cm")),
        legend.direction = "horizontal",
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(margin = margin(t = -5)),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = "#cbcbcb", size = 0.25))
  
}

# Total
subset <- Plot.dt.g[`Variable` == "Total"]

ggplot(data = subset,
       aes(x = `Time`, y = `Value`, color = `Variable`)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10),
                     labels = scales::comma,
                     limits = c(plyr::round_any(min(subset$Value), accuracy = 1000, f = floor),
                                plyr::round_any(max(subset$Value), accuracy = 1000, f = ceiling))) +
  geom_hline(yintercept = plyr::round_any(min(subset$Value), accuracy = 1000, f = floor),
             size = 0.5, colour="black") +
  theme_x() +
  scale_color_jco()

ggsave("Total.png")

# State
subset <- Plot.dt.g[str_detect(`Variable`, "^state/")]

ggplot(data = subset,
       aes(x = `Time`, y = `Value`, color = `Variable`)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10),
                     labels = scales::comma,
                     limits = c(plyr::round_any(min(subset$Value), accuracy = 1000, f = floor),
                                plyr::round_any(max(subset$Value), accuracy = 1000, f = ceiling))) +
  geom_hline(yintercept = plyr::round_any(min(subset$Value), accuracy = 1000, f = floor),
             size = 0.5, colour="black") +
  theme_x() +
  scale_color_d3()

ggsave("State.png")

# Sex
subset <- Plot.dt.g[str_detect(`Variable`, "^sex/")]

ggplot(data = subset,
       aes(x = `Time`, y = `Value`, color = `Variable`)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10),
                     labels = scales::comma) +
  theme_x() +
  facet_grid(`Variable`~., scales = "free_y") +
  scale_color_d3()

ggsave("Sex.png")

# Legal
subset <- Plot.dt.g[str_detect(`Variable`, "^leg/")]

ggplot(data = subset,
       aes(x = `Time`, y = `Value`, color = `Variable`)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10),
                     labels = scales::comma) +
  geom_hline(yintercept = plyr::round_any(min(subset$Value), accuracy = 1000, f = floor),
             size = 0.5, colour="black") +
  theme_x() +
  scale_color_d3()

ggsave("Legal.png")

# Indigenous
subset <- Plot.dt.g[str_detect(`Variable`, "^ind/")]

ggplot(data = subset,
       aes(x = `Time`, y = `Value`, color = `Variable`)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10),
                     labels = scales::comma,
                     limits = c(4000,
                                30000)) +
  geom_hline(yintercept = 4000,
             size = 0.5, colour="black") +
  theme_x() +
  scale_color_d3()

ggsave("Indigenous.png")


#### Functions and S matrices ####

#--- Shrink function ---#
shrink.estim <- function(x)
{
  n <- nrow(x)
  tar<-diag(apply(x, 2, crossprod) / n)
  if (is.matrix(x) == TRUE && is.numeric(x) == FALSE)
    stop("The data matrix must be numeric!", call. = FALSE)
  p <- ncol(x)
  n <- nrow(x)
  covm <- crossprod(x) / n
  corm <- cov2cor(covm)
  xs <- scale(x, center = FALSE, scale = sqrt(diag(covm)))
  v <- (1/(n * (n - 1))) * (crossprod(xs^2) - 1/n * (crossprod(xs))^2)
  diag(v) <- 0
  corapn <- cov2cor(tar)
  d <- (corm - corapn)^2
  lambda <- sum(v)/sum(d)
  lambda <- max(min(lambda, 1), 0)
  shrink.cov <- lambda * tar + (1 - lambda) * covm
  return(shrink.cov)
}

#--- Temporal S matrix ---#
S <- matrix(c(1,1,1,1, #Annual
              1,1,0,0, #Semi-annual
              0,0,1,1,
              diag(4)), nrow=4) %>% t

#### Loop ####

dt <- data.table("Time" = character(),
                  "Series" = character(),
                  "F-method" = character(),
                  "Temp R-method" = character(),
                  "CS R-method" = character(),
                  "Fc Horizon" = integer(),
                  "Forecasts" = double(),
                  "Actual" = double(),
                  "Frequency" = character(),
                  "Weights" = double(),
                  "Test Set Length" = double())

setkey(dt, `Test Set Length`)
setindexv(dt, c("F-method", "Temp R-method", "CS R-method", "Frequency"))

key(dt)
indices(dt)

aggts(data.y)[,1] %>% window(., start=c(2014,1)) %>% length() -> test_length

H = 3 # Annual forecast horizon (as it is the lowest frequency)

tststart = c(2014,1) # First test set start date

# Training sets, test sets and forecasts for each window[a], series[i] and frequency[x]

print("Training Set")
train <- foreach(a = 1:test_length, .packages = c("tidyverse", "thief", "fpp2", "hts"), .inorder = TRUE) %:%
  foreach(i = 1:ncol(aggts(data.y)), .packages = c("tidyverse", "thief", "fpp2", "hts"), .inorder=TRUE) %dopar% {
    
    aggts(data.y)[,i] %>% window(., end=tststart-c(0,2)+c(0,a)) %>% tsaggregates()
    
  }


print("Test Set")
test <- foreach(a = 1:test_length, .packages = c("tidyverse", "thief", "fpp2", "hts"), .inorder = TRUE) %:%
  foreach(i = 1:ncol(aggts(data.y)), .packages = c("tidyverse", "thief", "fpp2", "hts"),.inorder=TRUE) %dopar% {
    
    aggts(data.y)[,i] %>% window(., end=tststart-c(0,2)+c(0,a)) -> y.train
    
    aggts(data.y)[,i] %>%
      window(., start=(c(time(y.train)[length(y.train)])+1/4),
             end=min(c(time(y.train)[length(y.train)])+H,
                     time(aggts(data.y)[,i])[length(aggts(data.y)[,i])])) %>%
      tsaggregates()
    
  }


print("ARIMA Forecasts")
fc.ari <- foreach(a = 1:length(train), .packages = c("tidyverse", "thief", "fpp2", "hts"), .inorder = TRUE) %:%
  foreach(i = 1:ncol(aggts(data.y)), .packages = c("tidyverse", "thief", "fpp2", "hts"), .inorder=TRUE) %:%
  foreach(x = seq_along(train[[a]][[i]]), .packages = c("tidyverse", "thief", "fpp2", "hts"), .inorder=TRUE) %dopar% {
    
    auto.arima(train[[a]][[i]][[x]]) %>%
      forecast(., h=(H*frequency(train[[a]][[i]][[x]])))
    
  }


print("ETS Forecasts")
fc.ets <- foreach(a = 1:length(train), .packages = c("tidyverse", "thief", "fpp2", "hts"), .inorder = TRUE) %:%
  foreach(i = 1:ncol(aggts(data.y)), .packages = c("tidyverse", "thief", "fpp2", "hts"), .inorder=TRUE) %:%
  foreach(x = seq_along(train[[a]][[i]]), .packages = c("tidyverse", "thief", "fpp2", "hts"), .inorder=TRUE) %dopar% {
    
    ets(train[[a]][[i]][[x]]) %>%
      forecast(., h=(H*frequency(train[[a]][[i]][[x]])))
    
  }

if (getDoParWorkers() > 1){
  
  # Closing the parallel back end
  stopCluster(cl)
  registerDoSEQ()
  
}

for (a in 1:length(train)){
  print(paste("Window =", a))
  
  # Initialization of an intermediate data table
  int.m <- data.table("Time" = character(),
                      "Series" = character(),
                      "F-method" = character(),
                      "Temp R-method" = character(),
                      "CS R-method" = character(),
                      "Fc Horizon" = integer(),
                      "Forecasts" = double(),
                      "Actual" = double(),
                      "Frequency" = character(),
                      "Weights" = double(),
                      "Test Set Length" = double())
  
  setkey(int.m, `Test Set Length`)
  setindexv(int.m, c("F-method", "Temp R-method", "CS R-method", "Frequency"))
  
  # This allows performance increases in the later windows, as we would no longer be
  # rbinding to a data table with millions of observations and rbind creates a copy of the
  # huge dataframe every time it needs to add/append a row.
  
  ## Initialization of error matrices
  # Annual
  ann.ets <- matrix(NaN, nrow=length(train[[a]][[1]][[3]]), ncol=ncol(aggts(data.y)))
  ann.ari <- matrix(NaN, nrow=length(train[[a]][[1]][[3]]), ncol=ncol(aggts(data.y)))
  
  # Semi-annual
  biann.ets <- matrix(NaN, nrow=length(train[[a]][[1]][[2]]), ncol=ncol(aggts(data.y)))
  biann.ari <- matrix(NaN, nrow=length(train[[a]][[1]][[2]]), ncol=ncol(aggts(data.y)))
  
  # Quarterly
  qrtrly.ets <- matrix(NaN, nrow=length(train[[a]][[1]][[1]]), ncol=ncol(aggts(data.y)))
  qrtrly.ari <- matrix(NaN, nrow=length(train[[a]][[1]][[1]]), ncol=ncol(aggts(data.y)))
  
  #--- Base forecasts ---#
  
  # ARIMA
  print("Base ARIMA - Data Table addition")
  
  for(i in seq_along(fc.ari[[a]])){
    if (length(fc.ari[[a]][[i]]) == length(test[[a]][[i]])){
      for(x in 1:length(fc.ari[[a]][[i]])){
        for(h in 1:length(fc.ari[[a]][[i]][[x]]$mean)){
          
          data.table("Time" = paste(as.yearqtr(time(fc.ari[[a]][[i]][[x]]$mean)[h])),
                     "Series" = paste(colnames(aggts(data.y))[i]),
                     "F-method" = paste("ARIMA"),
                     "Temp R-method" = paste("None"),
                     "CS R-method" = paste("None"),
                     "Fc Horizon" = (1:length(fc.ari[[a]][[i]][[x]]$mean))[h],
                     "Forecasts" = as.vector(fc.ari[[a]][[i]][[x]]$mean)[h],
                     "Actual" = max(NULL, as.vector(test[[a]][[i]][[x]][h])),
                     "Frequency" = names(train[[a]][[i]])[[x]],
                     "Weights" = var(train[[a]][[i]][[x]]-fc.ari[[a]][[i]][[x]]$fitted),
                     "Test Set Length" = a) -> int
          
          rbind(int.m, int) -> int.m
        
        }
      }
    }
    else {
      for(x in 1:length(test[[a]][[i]])){
        for(h in 1:length(fc.ari[[a]][[i]][[x]]$mean)){
          
          data.table("Time" = paste(as.yearqtr(time(fc.ari[[a]][[i]][[x]]$mean)[h])),
                     "Series" = paste(colnames(aggts(data.y))[i]),
                     "F-method" = paste("ARIMA"),
                     "Temp R-method" = paste("None"),
                     "CS R-method" = paste("None"),
                     "Fc Horizon" = (1:length(fc.ari[[a]][[i]][[x]]$mean))[h],
                     "Forecasts" = as.vector(fc.ari[[a]][[i]][[x]]$mean)[h],
                     "Actual" = max(NULL, as.vector(test[[a]][[i]][[x]][h])),
                     "Frequency" = names(train[[a]][[i]])[[x]],
                     "Weights" = var(train[[a]][[i]][[x]]-fc.ari[[a]][[i]][[x]]$fitted),
                     "Test Set Length" = a) -> int
          
          rbind(int.m, int) -> int.m
          
        }
      }
      
      for (x in (length(test[[a]][[i]])+1):length(fc.ari[[a]][[i]])){
        
        data.table("Time" = paste(as.yearqtr(time(fc.ari[[a]][[i]][[x]]$mean))),
                   "Series" = paste(colnames(aggts(data.y))[i]),
                   "F-method" = paste("ARIMA"),
                   "Temp R-method" = paste("None"),
                   "CS R-method" = paste("None"),
                   "Fc Horizon" = 1:length(fc.ari[[a]][[i]][[x]]$mean),
                   "Forecasts" = as.vector(fc.ari[[a]][[i]][[x]]$mean),
                   "Actual" = NA,
                   "Frequency" = names(train[[a]][[i]])[[x]],
                   "Weights" = var(train[[a]][[i]][[x]]-fc.ari[[a]][[i]][[x]]$fitted),
                   "Test Set Length" = a) -> int
        
        rbind(int.m, int) -> int.m
        
      }
    }
    
    qrtrly.ari[,i] <- (train[[a]][[i]][[1]] - fc.ari[[a]][[i]][[1]]$fitted)
    biann.ari[,i] <- (train[[a]][[i]][[2]] - fc.ari[[a]][[i]][[2]]$fitted)
    ann.ari[,i] <- (train[[a]][[i]][[3]] - fc.ari[[a]][[i]][[3]]$fitted)
    
  }
  
  # ETS
  print("Base ETS - Data Table addition")
  
  for(i in seq_along(fc.ets[[a]])){
    if (length(fc.ets[[a]][[i]]) == length(test[[a]][[i]])) {
      for(x in 1:length(fc.ets[[a]][[i]])){
        for(h in 1:length(fc.ets[[a]][[i]][[x]]$mean)){
        
          data.table("Time" = paste(as.yearqtr(time(fc.ets[[a]][[i]][[x]]$mean)[h])),
                     "Series" = paste(colnames(aggts(data.y))[i]),
                     "F-method" = paste("ETS"),
                     "Temp R-method" = paste("None"),
                     "CS R-method" = paste("None"),
                     "Fc Horizon" = (1:length(fc.ets[[a]][[i]][[x]]$mean))[h],
                     "Forecasts" = as.vector(fc.ets[[a]][[i]][[x]]$mean)[h],
                     "Actual" = max(NULL, as.vector(test[[a]][[i]][[x]][h])),
                     "Frequency" = names(train[[a]][[i]])[[x]],
                     "Weights" = var(train[[a]][[i]][[x]]-fc.ets[[a]][[i]][[x]]$fitted),
                     "Test Set Length" = a) -> int
          
          rbind(int.m, int) -> int.m
        
        }
      }
    }
    else {
      for(x in 1:length(test[[a]][[i]])){
        for(h in 1:length(fc.ets[[a]][[i]][[x]]$mean)){
          
          data.table("Time" = paste(as.yearqtr(time(fc.ets[[a]][[i]][[x]]$mean)[h])),
                     "Series" = paste(colnames(aggts(data.y))[i]),
                     "F-method" = paste("ETS"),
                     "Temp R-method" = paste("None"),
                     "CS R-method" = paste("None"),
                     "Fc Horizon" = (1:length(fc.ets[[a]][[i]][[x]]$mean))[h],
                     "Forecasts" = as.vector(fc.ets[[a]][[i]][[x]]$mean)[h],
                     "Actual" = max(NULL, as.vector(test[[a]][[i]][[x]][h])),
                     "Frequency" = names(train[[a]][[i]])[[x]],
                     "Weights" = var(train[[a]][[i]][[x]]-fc.ets[[a]][[i]][[x]]$fitted),
                     "Test Set Length" = a) -> int
          
          rbind(int.m, int) -> int.m
          
        }
      }
      
      for (x in (length(test[[a]][[i]])+1):length(fc.ets[[a]][[i]])){
        
        data.table("Time" = paste(as.yearqtr(time(fc.ets[[a]][[i]][[x]]$mean))),
                   "Series" = paste(colnames(aggts(data.y))[i]),
                   "F-method" = paste("ETS"),
                   "Temp R-method" = paste("None"),
                   "CS R-method" = paste("None"),
                   "Fc Horizon" = 1:length(fc.ets[[a]][[i]][[x]]$mean),
                   "Forecasts" = as.vector(fc.ets[[a]][[i]][[x]]$mean),
                   "Actual" = NA,
                   "Frequency" = names(train[[a]][[i]])[[x]],
                   "Weights" = var(train[[a]][[i]][[x]]-fc.ets[[a]][[i]][[x]]$fitted),
                   "Test Set Length" = a) -> int
        
        rbind(int.m, int) -> int.m
        
      }
    }
    
    qrtrly.ets[,i] <- (train[[a]][[i]][[1]] - fc.ets[[a]][[i]][[1]]$fitted)
    biann.ets[,i] <- (train[[a]][[i]][[2]] - fc.ets[[a]][[i]][[2]]$fitted)
    ann.ets[,i] <- (train[[a]][[i]][[3]] - fc.ets[[a]][[i]][[3]]$fitted)
    
  }
  
  setkey(int.m, `Test Set Length`)
  setindexv(int.m, c("F-method", "Temp R-method", "CS R-method", "Frequency", "Series"))
  
  print("Temporal reconciliation")
  
  #--- Temporal reconciliation ---#
  unique(int.m$Series) -> series
  unique(int.m$`F-method`) -> fmeth
  
  for (f in fmeth){
    int.m[`F-method` == f & `Temp R-method` == "None" &
            `CS R-method` == "None"] -> fltrx
    
    setkey(fltrx, `Series`)
    setindexv(fltrx, c("Frequency", "Fc Horizon"))
    
    for (i in series){
      print(paste(f,i))
      
      for (ah in 1:H){
        
        # (Annual Horizon * Frequency - (Frequency-1)) : (Annual Horizon * Frequency)
        
        rbind(fltrx[`Series` == i &`Frequency` == "Annual" & `Fc Horizon` == ah][order(`Fc Horizon`)],
              fltrx[`Series` == i & `Frequency` == "Biannual" & `Fc Horizon` %in% ((ah*2-1):(ah*2))][order(`Fc Horizon`)],
              fltrx[`Series` == i & `Frequency` == "Quarterly" & `Fc Horizon` %in% ((ah*4-3):(ah*4))][order(`Fc Horizon`)]) -> fltr
        
        fltr[,`Forecasts`] -> yhat
        
        # Variance scaling
        fltr[,`Weights`] %>%
          Diagonal(n=length(yhat)) -> W
        
        W.inv <- solve(W)
        S.pr <- t(S)  
        (S %*% solve(S.pr %*% W.inv %*% S) %*% S.pr %*% W.inv %*% yhat) %>%
          as.vector() -> fc
        
        fltr[, !c("Forecasts", "Weights", "Temp R-method")] %>%
          cbind(., "Forecasts" = fc, "Weights" = NaN, "Temp R-method" = "Var") %>%
          rbind(int.m,.) -> int.m
        
        # Structural scaling
        uvec <- c(rep(1, 4))
        
        W <- (S %*% uvec) %>%
          Diagonal(n=7)
        
        W.inv <- solve(W)
        S.pr <- t(S)
        (S %*% solve(S.pr %*% W.inv %*% S) %*% S.pr %*% W.inv %*% yhat) %>%
          as.vector() -> fc
        
        fltr[, !c("Forecasts", "Weights", "Temp R-method")] %>%
          cbind(., "Forecasts" = fc, "Weights" = NaN, "Temp R-method" = "Struc") %>%
          rbind(int.m,.) -> int.m 
        
      }
    }
  }
  
  print("Cross-sectional reconciliation")
  
  #--- Cross-sectional reconciliation ---#
  
  colnames(aggts(data.y)) -> order.cs
  unique(int.m$`F-method`) -> fmeth
  c("None", "Var", "Struc") -> temp.rec
  unique(int.m$Frequency) -> freq
  
  list("Annual ARIMA" = ann.ari, "Annual ETS" = ann.ets,
       "Biannual ARIMA" = biann.ari, "Biannual ETS" = biann.ets,
       "Quarterly ARIMA"=qrtrly.ari, "Quarterly ETS"=qrtrly.ets) -> lst
  
  for (f in fmeth){
    for(u in temp.rec){
      
      # Initialization of lists required for Var-A and Mint-A
      lst.VarA <- list()
      lst.MinTA <- list()
      
      for (fr in freq){
        print(paste(f, u, fr))
        
        int.m[`F-method` == f & `Temp R-method` == u &
                `CS R-method` == "None" & `Frequency` == fr] -> fltrx
        setkey(fltrx, `Fc Horizon`)
        
        fltrx[order(`Fc Horizon`),`Fc Horizon`] %>% unique() -> fchor
        
        for (h in fchor){
          
          fltrx[`Fc Horizon` == h][order(match(`Series`,order.cs))] -> fltr
          fltr[,`Forecasts`] -> yhat
          
          # Required extraction from the list for errors
          paste(fr, f) -> x
          
          # Var
          extract2(lst, x) %>% cov() %>% diag() %>% Diagonal(n=243, x=.) -> W
          
          W.inv <- solve(W)
          Sc.pr <- t(Sc)  
          (Sc %*% solve(Sc.pr %*% W.inv %*% Sc) %*% Sc.pr %*% W.inv %*% yhat) %>%
            as.vector() -> fc
          
          fltr[, !c("Forecasts", "Weights", "CS R-method")] %>%
            cbind(., "Forecasts" = fc, "Weights" = NaN, "CS R-method" = "Var") %>%
            rbind(int.m, .) -> int.m
          
          (solve(Sc.pr %*% W.inv %*% Sc) %*% Sc.pr %*% W.inv) -> lst.VarA[[fr]]
          
          # MinT
          extract2(lst,x) %>% shrink.estim() -> W
          W.inv <- solve(W)
          Sc.pr <- t(Sc)
          (Sc %*% solve(Sc.pr %*% W.inv %*% Sc) %*% Sc.pr %*% W.inv %*% yhat) %>%
            as.vector() -> fc
          
          fltr[, .(`Time`, `Series`, `F-method`, `Temp R-method`, `Fc Horizon`, `Actual`,
                   `Frequency`, `Test Set Length`)] %>%
            cbind(., "Forecasts" = fc, "Weights" = NaN, "CS R-method" = "MinT") %>%
            rbind(int.m, .) -> int.m
          
          (solve(Sc.pr %*% W.inv %*% Sc) %*% Sc.pr %*% W.inv) -> lst.MinTA[[fr]]
          
        }
      }
      
      # Var-A
      Reduce(`+`, lst.VarA)/length(lst.VarA) -> Gbar.VarA
      
      # MinT-A
      Reduce(`+`, lst.MinTA)/length(lst.MinTA) -> Gbar.MinTA
      
      for (fr in freq){
        print(paste("Var-A & MinT-A:", fr))
        
        int.m[`F-method` == f & `Temp R-method` == u &
                `CS R-method` == "None" & `Frequency` == fr] -> fltrx
        setkey(fltrx, `Fc Horizon`)
        
        fltrx[order(`Fc Horizon`),`Fc Horizon`] %>% unique() -> fchor
        
        for (h in fchor){
          
          fltrx[`Fc Horizon` == h][order(match(`Series`, order.cs))] -> fltr 
          fltr[,`Forecasts`] -> yhat
          
          # Var-A
          (Sc %*% Gbar.VarA %*% yhat) %>% as.vector() -> fc
          
          fltr[, !c("Forecasts", "Weights", "CS R-method")] %>%
            cbind(., "Forecasts" = fc, "Weights" = NaN, "CS R-method" = "Var-A") %>%
            rbind(int.m, .) -> int.m
          
          # MinT-A
          (Sc %*% Gbar.MinTA %*% yhat) %>% as.vector() -> fc
          
          fltr[, !c("Forecasts", "Weights", "CS R-method")] %>%
            cbind(., "Forecasts" = fc, "Weights" = NaN, "CS R-method" = "MinT-A") %>%
            rbind(int.m, .) -> int.m
          
        }
      }
    }
  }
  
  rbind(dt, int.m) -> dt
  # Rbinding the intermediate data table to the main data table at the end of each window
}

View(dt)

#### Error measures ####

na.omit(dt, cols = "Actual") %>%
  .[, c("AbsPerError", "SqrdError") :=
      .(abs((Actual-Forecasts)/Actual*100), (Actual-Forecasts)^2)] -> Summt

# NEED TO INVESTIGATE. Some series were perfectly forecasted by ARIMA.
Summt[SqrdError == 0] %>% View()

#### Table 3 ####
Summt[,.(`MSE` = mean(SqrdError)),
      by=.(`Temp R-method`, `CS R-method`, `F-method`, `Series`,`Test Set Length`)] %>%
  dcast(.,
        `Temp R-method` + `CS R-method` + `F-method` + `Test Set Length` ~ `Series`,
        value.var="MSE") -> Summx

setkey(Summx, `Test Set Length`)
setindex(Summx, `F-method`)

unique(Summx$`Test Set Length`) -> wind
unique(Summx$`F-method`) -> fmeth

data.table("Temp R-method" = character(),
           "CS R-method" = character(),
           "F-method" = character(),
           "Series" = character(),
           "Test Set Length" = double(),
           "MSE" = double()) -> Tb3x

for (f in fmeth){
  for (w in wind){
    print(paste(f, w))
    
    matrix(c(NaN), nrow=15, ncol=243) -> matx
    
    Summx[`F-method` == f & `Test Set Length` == w] -> Summ1
    
    Summ1[`CS R-method` == "None" &
            `Temp R-method` == "None"][,c("Temp R-method", "CS R-method",
                                          "F-method", "Test Set Length") := NULL] %>%
      as_vector() -> Base
    
    Summ1[,c("Temp R-method", "CS R-method", "F-method", "Test Set Length") := NULL] %>%
      as.matrix() -> Summ.Mat
    
    for (r in 1:nrow(Summ.Mat)){
      Summ.Mat[r,]/Base -> matx[r,]
    }
    
    colnames(matx) <- colnames(Summ.Mat)
    
    Summx[`F-method` == f &
            `Test Set Length` == w][,.(`Temp R-method`, `CS R-method`,
                                       `F-method`, `Test Set Length`)] %>%
      cbind(., matx) %>%
      melt(., id.vars = c("Temp R-method", "CS R-method", "F-method", "Test Set Length"),
           variable.name = "Series",
           value.name = "MSE") %>%
      rbind(Tb3x, .) -> Tb3x
    
  }
}

Tb3x[is.numeric(MSE) & is.finite(MSE)] %>%
  .[, .("AvgRelMSE" = exp(mean(log(MSE)))), by=.(`Temp R-method`, `CS R-method`, `F-method`)] %>%
  dcast(., `Temp R-method` + `CS R-method` ~ `F-method`, value.var = "AvgRelMSE") %>%
  .[order(match(`Temp R-method`, c("None", "Var", "Struc")),
          match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] -> Tb3.Hie

Tb3x[!str_detect(Series, "Total") & !str_detect(Series, "^state/") &
       !str_detect(Series, "^sex/") & !str_detect(Series, "^leg/") &
       !str_detect(Series, "^ind/") & !str_detect(Series, "^state_sex/") &
       !str_detect(Series, "^state_leg/") & !str_detect(Series, "^state_ind/") &
       !str_detect(Series, "^sex_leg/") & !str_detect(Series, "^sex_ind/") &
       !str_detect(Series, "^ind_leg/") & !str_detect(Series, "^state_sex_leg/") &
       !str_detect(Series, "^state_sex_ind/") & !str_detect(Series, "^state_leg_ind/") &
       !str_detect(Series, "^sex_leg_ind/")][is.numeric(MSE) & is.finite(MSE)] %>%
  .[, .("AvgRelMSE" = exp(mean(log(MSE)))), by=.(`Temp R-method`, `CS R-method`, `F-method`)] %>%
  dcast(., `Temp R-method` + `CS R-method` ~ `F-method`, value.var = "AvgRelMSE") %>%
  .[order(match(`Temp R-method`, c("None", "Var", "Struc")),
          match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] -> Tb3.Bot

knitr::kable(Tb3.Hie, digits = 3, row.names = F, booktabs = T)
knitr::kable(Tb3.Bot, digits = 3, row.names = F, booktabs = T)


# Graph
rbind(cbind(Tb3.Hie, "Level" = "Hierarchy"),
cbind(Tb3.Bot, "Level" = "Bottom")) %>%
  melt(., id.var = c("Temp R-method", "CS R-method", "Level"),
       variable.name = "F-method",
       value.name = "AvgRelMSE") -> Tb3.Plot.Dt

Tb3.Plot.Dt$`CS R-method` <- factor(Tb3.Plot.Dt$`CS R-method`, levels = c("None", "Var", "Var-A",
                                                                            "MinT", "MinT-A"))

Tb3.Plot.Dt[`F-method` == "ETS"] %>%
  ggplot(., aes(x = `CS R-method`, y = `AvgRelMSE`, color = `Level`, group = `Level`)) +
  geom_point(aes(shape = `Level`), size = 3) +
  geom_line() +
  facet_grid(~`Temp R-method`) +
  scale_color_manual(values = c("steelblue", "indianred")) +
  labs(title = "Temporal Reconciliation",
       x = "Cross-sectional Reconciliation method:",
       color = "Series:",
       shape = "Series:",
       tag = "ETS") +
  theme(plot.title = element_text(hjust = 0.5, size = 11),
        axis.title = element_text(size = 11)) -> P1x

Tb3.Plot.Dt[`F-method` == "ARIMA"] %>%
  ggplot(., aes(x = `CS R-method`, y = `AvgRelMSE`, color = `Level`, group = `Level`)) +
  geom_point(aes(shape = `Level`), size = 3) +
  geom_line() +
  facet_grid(~`Temp R-method`) +
  scale_color_manual(values = c("steelblue", "indianred")) +
  labs(title = "Temporal Reconciliation",
       x = "Cross-sectional Reconciliation method:",
       color = "Series:",
       shape = "Series:",
       tag = "ARIMA") +
  theme(plot.title = element_text(hjust = 0.5, size = 11),
        axis.title = element_text(size = 11))  -> P2x
  
grid.arrange(P1x, P2x)



#### Table a: Temporal levels ####
Summt[, .("MSE"=mean(SqrdError)),
      by=.(`Temp R-method`, `CS R-method`, `F-method`, `Series`,
           `Test Set Length`, `Frequency`)] %>%
  dcast(., `Temp R-method` + `CS R-method` + `F-method` + `Test Set Length` + `Frequency` ~ `Series`,
        value.var = c("MSE")) -> Summx

unique(Summx$`Test Set Length`) -> wind
unique(Summx$`F-method`) -> fmeth

data.table("Temp R-method" = character(),
           "CS R-method" = character(),
           "F-method" = character(),
           "Series" = character(),
           "Test Set Length" = double(),
           "Frequency" = character(),
           "MSE" = double()) -> Tbax

for (f in fmeth){
  for (w in wind){
    
    data.table("Temp R-method" = character(),
               "CS R-method" = character(),
               "F-method" = character(),
               "Series" = character(),
               "Test Set Length" = double(),
               "Frequency" = character(),
               "MSE" = double()) -> int
    
    Summx[`F-method` == f & `Test Set Length` == w] -> fltrx
    fltrx[,`Frequency`] %>% unique() -> freq
    
    setkey(fltrx, `Frequency`)
    
    for (fr in freq){
      print(paste(f, w, fr))
      
      matrix(c(NaN), nrow=15, ncol=243) -> matx
      fltrx[`Frequency` == fr] -> Summ1
      
      Summ1[`CS R-method` == "None" &
              `Temp R-method` == "None"][,c("Temp R-method", "CS R-method",
                                            "F-method", "Test Set Length",
                                            "Frequency") := NULL] %>%
        as_vector() -> Base
      
      Summ1[,c("Temp R-method", "CS R-method", "F-method", "Test Set Length",
               "Frequency") := NULL] %>%
        as.matrix() -> Summ.Mat
      
      for (r in 1:nrow(Summ.Mat)){
        Summ.Mat[r,]/Base -> matx[r,]
      }
      
      colnames(matx) <- colnames(Summ.Mat)
      
      fltrx[`Frequency` == fr][,.(`Temp R-method`, `CS R-method`,
                                  `F-method`, `Test Set Length`, `Frequency`)] %>%
        cbind(., matx) %>%
        melt(., id.vars = c("Temp R-method", "CS R-method", "F-method", "Test Set Length", "Frequency"),
             variable.name = "Series",
             value.name = "MSE") %>%
        rbind(int, .) -> int
      
    }
    
    rbind(Tbax, int) -> Tbax
    
  }
}

Tbax[is.numeric(MSE) & is.finite(MSE)] %>%
  .[, .("AvgRelMSE" = exp(mean(log(MSE)))), by=.(`Temp R-method`, `CS R-method`,
                                                `F-method`,`Frequency`)] %>%
  dcast(.,`F-method` + `Temp R-method` + `CS R-method` ~ `Frequency`,
        value.var = "AvgRelMSE") %>%
  .[order(match(`F-method`, c("ARIMA", "ETS")),
          match(`Temp R-method`, c("None", "Var", "Struc")),
          match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] -> Tba

Tba[, c("F-method", "Temp R-method", "CS R-method",
        "Annual", "Biannual", "Quarterly")] -> Tba

knitr::kable(Tba, digits = 3, row.names = F, booktabs = T)


#### Table b: Forecast Horizons ####
Summt[, .("MSE" = mean(SqrdError)),
      by = .(`Temp R-method`, `CS R-method`, `F-method`, `Series`,
             `Test Set Length`, `Fc Horizon`)] %>%
  dcast(., `Temp R-method` + `CS R-method` + `F-method` + `Test Set Length` +
          `Fc Horizon` ~ `Series`,
        value.var = "MSE") -> Summx

unique(Summx$`Test Set Length`) -> wind
unique(Summx$`F-method`) -> fmeth

data.table("Temp R-method" = character(),
           "CS R-method" = character(),
           "F-method" = character(),
           "Series" = character(),
           "Test Set Length" = double(),
           "Fc Horizon" = double(),
           "MSE" = double()) -> Tbbx

for (f in fmeth){
  for (w in wind){
    
    data.table("Temp R-method" = character(),
               "CS R-method" = character(),
               "F-method" = character(),
               "Series" = character(),
               "Test Set Length" = double(),
               "Fc Horizon" = double(),
               "MSE" = double()) -> int
    
    Summx[`F-method` == f & `Test Set Length` == w] -> fltrx
    setkey(fltrx, `Fc Horizon`)
    
    fltrx[,`Fc Horizon`] %>% unique() -> fchor
    
    for (h in fchor){
      print(paste(f, w, h))
      
      matrix(c(NaN), nrow=15, ncol=243) -> matx
      
      fltrx[`Fc Horizon` == h] -> Summ1
      
      Summ1[`CS R-method` == "None" &
              `Temp R-method` == "None"][,c("Temp R-method", "CS R-method", "F-method",
                                            "Test Set Length", "Fc Horizon") := NULL] %>%
        as_vector() -> Base
      
      Summ1[,c("Temp R-method", "CS R-method", "F-method", "Test Set Length",
               "Fc Horizon") := NULL] %>%
        as.matrix() -> Summ.Mat
      
      for (r in 1:nrow(Summ.Mat)){
        Summ.Mat[r,]/Base -> matx[r,]
      }
      
      colnames(matx) <- colnames(Summ.Mat)
      
      fltrx[`Fc Horizon` == h][,.(`Temp R-method`, `CS R-method`,
                                  `F-method`, `Test Set Length`, `Fc Horizon`)] %>%
        cbind(., matx) %>%
        melt(., id.vars = c("Temp R-method", "CS R-method", "F-method",
                            "Test Set Length", "Fc Horizon"),
             variable.name = "Series",
             value.name = "MSE") %>%
        rbind(int, .) -> int
    }
    
    rbind(Tbbx, int) -> Tbbx
    
  }
}

Tbbx[is.numeric(MSE) & is.finite(MSE)] %>%
  .[, .("AvgRelMSE" = exp(mean(log(MSE)))),
     by = .(`Temp R-method`, `CS R-method`, `F-method`, `Fc Horizon`)] %>%
  dcast(., `F-method` + `Temp R-method` + `CS R-method` ~ `Fc Horizon`,
        value.var = "AvgRelMSE") %>%
  .[order(match(`F-method`, c("ARIMA", "ETS")),
          match(`Temp R-method`, c("None", "Var", "Struc")),
          match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] -> Tbb

knitr::kable(Tbb, digits = 3, row.names = F, booktabs = T)


#### Table 4: MAPE % reduction ####

calc <- function(.data, .dt){
  
  # 1-4
  .data[(`Frequency` == "Quarterly" & `Fc Horizon` %in% 1:4) |
          (`Frequency` == "Biannual" & `Fc Horizon` %in% 1:2) |
          (`Frequency` == "Annual" & `Fc Horizon` == 1),
        .("1-4" = mean(AbsPerError)),
        by = .(`Series`, `Temp R-method`, `CS R-method`, `F-method`)] %>%
    .[order(match(`F-method`, c("ARIMA", "ETS")),
            match(`Temp R-method`, c("None", "Var", "Struc")),
            match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] -> Summx
  
  Summx[, `F-method`] %>% unique() -> fmeth
  
  for (f in fmeth){
    
    Summx[`F-method` == f] %>%
      dcast(., `Temp R-method` + `CS R-method` + `F-method` ~ `Series`,
            value.var = "1-4") %>%
      .[order(match(`F-method`, c("ARIMA", "ETS")),
              match(`Temp R-method`, c("None", "Var", "Struc")),
              match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] -> Summ.int
    
    Summ.int[`CS R-method` == "None" & `Temp R-method` == "None",
             !c("Temp R-method", "CS R-method", "F-method")] %>%
      as_vector() -> Base
    
    Summ.int[, !c("Temp R-method", "CS R-method", "F-method")] %>%
      as.matrix() -> Summ.mat
    
    matrix(c(NaN), nrow = nrow(Summ.mat), ncol = ncol(Summ.mat)) -> matx
    
    for (r in 1:nrow(Summ.mat)){
      
      (Summ.mat[r,]/Base-1)*100 -> matx[r,]
      
    }
    
    colnames(matx) <- colnames(Summ.mat)
    
    Summ.int[`CS R-method` == "None" & `Temp R-method` == "None",
             !c("Temp R-method", "CS R-method", "F-method")] %>%
      melt(., variable.name = "Series", value.name = "Base Error",
           measure.var = colnames(Summ.mat)) -> Base.Errors
    
    Summ.int[,.(`Temp R-method`, `CS R-method`, `F-method`)] %>%
      cbind(., "Fc Term" = "1-4", matx) %>%
      melt(., id.vars = c("Temp R-method", "CS R-method", "F-method", "Fc Term"),
           variable.name = "Series",
           value.name = "MAPE") %>%
      .[Base.Errors, on = .(Series = Series)] %>%
      rbind(.dt, .) -> .dt
    
  }
  
  # 5-8
  .data[(`Frequency` == "Quarterly" & `Fc Horizon` %in% 5:8) |
          (`Frequency` == "Biannual" & `Fc Horizon` %in% 3:4) |
          (`Frequency` == "Annual" & `Fc Horizon` == 2),
        .("5-8" = mean(AbsPerError)),
        by = .(`Series`, `Temp R-method`, `CS R-method`, `F-method`)] %>%
    .[order(match(`F-method`, c("ARIMA", "ETS")),
            match(`Temp R-method`, c("None", "Var", "Struc")),
            match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] -> Summx
  
  Summx[, `F-method`] %>% unique() -> fmeth
  
  for (f in fmeth){
    
    Summx[`F-method` == f] %>%
      dcast(., `Temp R-method` + `CS R-method` + `F-method` ~ `Series`,
            value.var = "5-8") %>%
      .[order(match(`F-method`, c("ARIMA", "ETS")),
              match(`Temp R-method`, c("None", "Var", "Struc")),
              match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] -> Summ.int
    
    Summ.int[`CS R-method` == "None" & `Temp R-method` == "None",
             !c("Temp R-method", "CS R-method", "F-method")] %>%
      as_vector() -> Base
    
    Summ.int[, !c("Temp R-method", "CS R-method", "F-method")] %>%
      as.matrix() -> Summ.mat
    
    matrix(c(NaN), nrow = nrow(Summ.mat), ncol = ncol(Summ.mat)) -> matx
    
    for (r in 1:nrow(Summ.mat)){
      
      (Summ.mat[r,]/Base-1)*100 -> matx[r,]
      
    }
    
    colnames(matx) <- colnames(Summ.mat)
    
    Summ.int[`CS R-method` == "None" & `Temp R-method` == "None",
             !c("Temp R-method", "CS R-method", "F-method")] %>%
      melt(., variable.name = "Series", value.name = "Base Error",
           measure.var = colnames(Summ.mat)) -> Base.Errors
    
    Summ.int[,.(`Temp R-method`, `CS R-method`, `F-method`)] %>%
      cbind(., "Fc Term" = "5-8", matx) %>%
      melt(., id.vars = c("Temp R-method", "CS R-method", "F-method", "Fc Term"),
           variable.name = "Series",
           value.name = "MAPE") %>%
      .[Base.Errors, on = .(Series = Series)] %>%
      rbind(.dt, .) -> .dt
    
    
  }
  
  # 9-12
  .data[(`Frequency` == "Quarterly" & `Fc Horizon` %in% 9:12) |
          (`Frequency` == "Biannual" & `Fc Horizon` %in% 5:6) |
          (`Frequency` == "Annual" & `Fc Horizon` == 3),
        .("9-12" = mean(AbsPerError)),
        by = .(`Series`, `Temp R-method`, `CS R-method`, `F-method`)] %>%
    .[order(match(`F-method`, c("ARIMA", "ETS")),
            match(`Temp R-method`, c("None", "Var", "Struc")),
            match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] -> Summx
  
  Summx[, `F-method`] %>% unique() -> fmeth
  
  for (f in fmeth){
    
    Summx[`F-method` == f] %>%
      dcast(., `Temp R-method` + `CS R-method` + `F-method` ~ `Series`,
            value.var = "9-12") %>%
      .[order(match(`F-method`, c("ARIMA", "ETS")),
              match(`Temp R-method`, c("None", "Var", "Struc")),
              match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] -> Summ.int
    
    Summ.int[`CS R-method` == "None" & `Temp R-method` == "None",
             !c("Temp R-method", "CS R-method", "F-method")] %>%
      as_vector() -> Base
    
    Summ.int[, !c("Temp R-method", "CS R-method", "F-method")] %>%
      as.matrix() -> Summ.mat
    
    matrix(c(NaN), nrow = nrow(Summ.mat), ncol = ncol(Summ.mat)) -> matx
    
    for (r in 1:nrow(Summ.mat)){
      
      (Summ.mat[r,]/Base-1)*100 -> matx[r,]
      
    }
    
    colnames(matx) <- colnames(Summ.mat)
    
    Summ.int[`CS R-method` == "None" & `Temp R-method` == "None",
             !c("Temp R-method", "CS R-method", "F-method")] %>%
      melt(., variable.name = "Series", value.name = "Base Error",
           measure.var = colnames(Summ.mat)) -> Base.Errors
    
    Summ.int[,.(`Temp R-method`, `CS R-method`, `F-method`)] %>%
      cbind(., "Fc Term" = "9-12", matx) %>%
      melt(., id.vars = c("Temp R-method", "CS R-method", "F-method", "Fc Term"),
           variable.name = "Series",
           value.name = "MAPE") %>%
      .[Base.Errors, on = .(Series = Series)] %>%
      rbind(.dt, .) -> .dt
    
    
    
  }
  
  return(.dt)
  
}

data.table("Series" = character(),
           "Temp R-method" = character(),
           "CS R-method" = character(),
           "F-method" = character(),
           "Fc Term" = character(),
           "Base Error" = character(),
           "MAPE" = double()) -> Tb4

# Total
Summt[`Series` == "Total"] -> Summ
calc(.data = Summ, .dt = Tb4) -> Tb4


# States
Summt[str_detect(Series, "^state/")] -> Summ
calc(Summ, Tb4) -> Tb4


# Sex
Summt[str_detect(Series, "^sex/")] -> Summ
calc(Summ, Tb4) -> Tb4


# Legal Status
Summt[str_detect(Series, "^leg/")] -> Summ
calc(Summ, Tb4) -> Tb4


# Indigenous status
Summt[str_detect(Series, "^ind/")] -> Summ
calc(Summ, Tb4) -> Tb4


## Two-way interactions
# State x Sex
copy(Summt) %>%
  .[str_detect(Series, "^state_sex")] %>%
  .[, c("Series") := .("State x Sex")] -> Summ

calc(Summ, Tb4) -> Tb4


# State x Legal
copy(Summt) %>%
  .[str_detect(Series, "^state_sex")] %>%
  .[, c("Series") := .("State x Legal")] -> Summ

calc(Summ, Tb4) -> Tb4

# State x Indigenous
copy(Summt) %>%
  .[str_detect(Series, "^state_ind")] %>%
  .[, c("Series") := .("State x Indigenous")] -> Summ

calc(Summ, Tb4) -> Tb4

# Sex x Legal
copy(Summt) %>%
  .[str_detect(Series, "^sex_leg/")] %>%
  .[, c("Series") := .("Sex x Legal")] -> Summ

calc(Summ, Tb4) -> Tb4

# Sex x Indigenous
copy(Summt) %>%
  .[str_detect(Series, "^sex_ind/")] %>%
  .[, c("Series") := .("Sex x Indigenous")] -> Summ

calc(Summ, Tb4) -> Tb4

# Indigenous x Legal
copy(Summt) %>%
  .[str_detect(Series, "^ind_leg/")] %>%
  .[, c("Series") := .("Indigenous x Legal")] -> Summ

calc(Summ, Tb4) -> Tb4

## Three-way interactions
# State x Sex x Legal
copy(Summt) %>%
  .[str_detect(Series, "^state_sex_leg/")] %>%
  .[, c("Series") := .("State x Sex x Legal")] -> Summ

calc(Summ, Tb4) -> Tb4

# State x Sex x Indigenous
copy(Summt) %>%
  .[str_detect(Series, "^state_sex_ind/")] %>%
  .[, c("Series") := .("State x Sex x Indigenous")] -> Summ

calc(Summ, Tb4) -> Tb4

# State x Legal x Indigenous
copy(Summt) %>%
  .[str_detect(Series, "^state_leg_ind/")] %>%
  .[, c("Series") := .("State x Legal x Indigenous")] -> Summ

calc(Summ, Tb4) -> Tb4

# Sex x Legal x Indigenous
copy(Summt) %>%
  .[str_detect(Series, "^sex_leg_ind/")] %>%
  .[, c("Series") := .("Sex x Legal x Indigenous")] -> Summ

calc(Summ, Tb4) -> Tb4

# Bottom-level: State x Sex x Legal x Indigenous
copy(Summt) %>%
  .[!str_detect(Series, "Total") & !str_detect(Series, "^state/") &
      !str_detect(Series, "^sex/") & !str_detect(Series, "^leg/") &
      !str_detect(Series, "^ind/") & !str_detect(Series, "^state_sex/") &
      !str_detect(Series, "^state_leg/") & !str_detect(Series, "^state_ind/") &
      !str_detect(Series, "^sex_leg/") & !str_detect(Series, "^sex_ind/") &
      !str_detect(Series, "^ind_leg/") & !str_detect(Series, "^state_sex_leg/") &
      !str_detect(Series, "^state_sex_ind/") & !str_detect(Series, "^state_leg_ind/") &
      !str_detect(Series, "^sex_leg_ind/")] %>%
  .[, c("Series") := .("State x Sex x Legal x Indigenous")] %>%
  .[is.finite(`AbsPerError`) & is.numeric(`AbsPerError`)] -> Summ

calc(Summ, Tb4) -> Tb4


#### Table 4: Analysis ####
Tb4[, `Base Error` := as.numeric(`Base Error`)]
str(Tb4)

Tb4[`F-method` == "ARIMA"] %>%
  dcast(., `Series` + `Temp R-method` + `CS R-method` ~ `Fc Term`,
        value.var = c("MAPE", "Base Error")) %>%
  .[order(match(`Series`, colnames(aggts(data.y))),
          match(`Temp R-method`, c("None", "Struc", "Var")),
          match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] %>%
  .[, c("Series", "Temp R-method", "CS R-method", "Base Error_1-4", "MAPE_1-4",
        "Base Error_5-8", "MAPE_5-8", "Base Error_9-12", "MAPE_9-12")] -> Tb4.Ari

Tb4[`F-method` == "ETS"] %>%
  dcast(., `Series` + `Temp R-method` + `CS R-method` ~ `Fc Term`,
        value.var = c("MAPE", "Base Error")) %>%
  .[order(match(`Series`, colnames(aggts(data.y))),
          match(`Temp R-method`, c("None", "Struc", "Var")),
          match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] %>%
  .[, c("Series", "Temp R-method", "CS R-method", "Base Error_1-4", "MAPE_1-4",
        "Base Error_5-8", "MAPE_5-8", "Base Error_9-12", "MAPE_9-12")] -> Tb4.Ets

knitr::kable(Tb4.Ari, digits = 2, booktabs = T)
knitr::kable(Tb4.Ets, digits = 2, booktabs = T)

Tb4 %>%
  melt(., id.vars = c("Series", "Temp R-method", "CS R-method", "F-method", "Fc Term"),
       variable.name = "Error Type",
       value.name = "Error") -> Plot.dt1

factor(Plot.dt1$Series, levels = rev(c("Total","state/NSW","state/QLD","state/VIC","state/SA","state/TAS","state/WA", "state/NT",
                                       "state/ACT", "sex/f", "sex/m", "leg/r", "leg/s", "ind/I", "ind/NI", "State x Legal",
                                       "State x Sex", "State x Indigenous", "Sex x Indigenous", "Sex x Legal", "Indigenous x Legal",
                                       "State x Legal x Indigenous", "State x Sex x Indigenous", "State x Sex x Legal",
                                       "Sex x Legal x Indigenous", "State x Sex x Legal x Indigenous"))) -> Plot.dt1$Series

# Option 1
Plot.dt1[`F-method` == "ETS" & `Temp R-method` == "Var" & `CS R-method` == "MinT"] %>%
  ggplot(., aes(y = `Error`, x = `Series`, fill = `Error Type`)) +
  geom_bar(stat='identity', position = position_dodge(), colour="black") +
  facet_wrap(~ `Fc Term`) +
  coord_flip() +
  labs(title = "ETS - T: Var, CS: MinT",
       fill = "Error Type:") -> P1

Plot.dt1[`F-method` == "ETS" & `Temp R-method` == "Var" & `CS R-method` == "MinT-A"] %>%
  ggplot(., aes(y = `Error`, x = `Series`, fill = `Error Type`)) +
  geom_bar(stat='identity', position = position_dodge(), colour="black") +
  facet_wrap(~ `Fc Term`) +
  coord_flip() +
  labs(title = "ETS - T: Var, CS: MinT-A",
       fill = "Error Type:") -> P2

grid.arrange(P1, P2)


# Option 2
Plot.dt1[`Error Type` == "Base Error" & `F-method` == "ETS" & `Temp R-method` == "None" &
           `CS R-method` == "None"] %>%
  ggplot(., aes(y = `Error`, x = `Series`)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", fill = "steelblue") +
  facet_wrap(~ `Fc Term`) +
  coord_flip() +
  labs(title = "ETS Base Errors") -> P3

Plot.dt1[`Error Type` == "MAPE" & `F-method` == "ETS" & `Temp R-method` == "Var" &
           (`CS R-method` == "MinT" | `CS R-method` == "MinT-A")] %>%
  ggplot(., aes(y = `Error`, x = `Series`, fill = `CS R-method`)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  facet_wrap(~ `Fc Term`) +
  coord_flip() +
  labs(title = "ETS with Var Temporal Reconciliation",
       fill = "CS R-method:") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("steelblue", "indianred1")) -> P4

grid.arrange(P3, P4)


# Option 3
Plot.dt1[`Error Type` == "Base Error" & `F-method` == "ETS" & `Temp R-method` == "None" &
           `CS R-method` == "None"& `Fc Term` == "1-4"] %>%
  ggplot(., aes(y = `Error`, x = `Series`)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", fill = "steelblue") +
  coord_flip() +
  labs(title = "Base Errors ETS (1-4)") -> P5


Plot.dt1[`F-method` == "ETS" & `Fc Term` == "1-4" & `Temp R-method` == "Var" &
           (`CS R-method` == "MinT" | `CS R-method` == "MinT-A") & `Error Type` == "MAPE"] %>%
  ggplot(., aes(y = `Error`, x = `Series`, fill = `CS R-method`)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  coord_flip() +
  labs(title = "ETS Var Temporal Reconciliation (1-4)",
       fill = "CS R-method:",
       y = "Percentage reduction") +
  scale_fill_manual(values = c("steelblue", "indianred")) -> P6

grid.arrange(P5, P6, nrow = 1)


Plot.dt1[`Error Type` == "Base Error" & `F-method` == "ETS" & `Temp R-method` == "None" &
           `CS R-method` == "None"& `Fc Term` == "5-8"] %>%
  ggplot(., aes(y = `Error`, x = `Series`)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", fill = "steelblue") +
  coord_flip() +
  labs(title = "Base Errors ETS (5-8)") -> P7


Plot.dt1[`F-method` == "ETS" & `Fc Term` == "5-8" & `Temp R-method` == "Var" &
           (`CS R-method` == "MinT" | `CS R-method` == "MinT-A") & `Error Type` == "MAPE"] %>%
  ggplot(., aes(y = `Error`, x = `Series`, fill = `CS R-method`)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  coord_flip() +
  labs(title = "ETS Var Temporal Reconciliation (5-8)",
       fill = "CS R-method:",
       y = "Percentage reduction") +
  scale_fill_manual(values = c("steelblue", "indianred")) -> P8

grid.arrange(P7, P8, nrow = 1)



Plot.dt1[`Error Type` == "Base Error" & `F-method` == "ETS" & `Temp R-method` == "None" &
           `CS R-method` == "None"& `Fc Term` == "9-12"] %>%
  ggplot(., aes(y = `Error`, x = `Series`)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black", fill = "steelblue") +
  coord_flip() +
  labs(title = "Base Errors ETS (9-12)") -> P9


Plot.dt1[`F-method` == "ETS" & `Fc Term` == "9-12" & `Temp R-method` == "Var" &
           (`CS R-method` == "MinT" | `CS R-method` == "MinT-A") & `Error Type` == "MAPE"] %>%
  ggplot(., aes(y = `Error`, x = `Series`, fill = `CS R-method`)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  coord_flip() +
  labs(title = "ETS Var Temporal Reconciliation (9-12)",
       fill = "CS R-method:",
       y = "Percentage reduction") +
  scale_fill_manual(values = c("steelblue", "indianred")) -> P10

grid.arrange(P9, P10, nrow = 1)


# Plot.dt1[`F-method` == "ETS" & !(`Temp R-method` == "None" & `CS R-method` == "None") &
#            `Error Type` == "MAPE" & `Series` == "Total"] %>%
#   ggplot(., aes(y = `Error`, x = `Series`, color = `Temp R-method`, fill = `CS R-method`)) +
#   geom_bar(stat = "identity", position = position_dodge(), size = 2) +
#   facet_wrap(~ `Fc Term`) +
#   coord_flip()


#### MAPE by windows ####

calc2 <- function(.data, .dt){
  
  # 1-4
  .data[(`Frequency` == "Quarterly" & `Fc Horizon` %in% 1:4) |
          (`Frequency` == "Biannual" & `Fc Horizon` %in% 1:2) |
          (`Frequency` == "Annual" & `Fc Horizon` == 1),
        .("1-4" = mean(AbsPerError)),
        by = .(`Series`, `Temp R-method`, `CS R-method`, `F-method`, `Test Set Length`)] %>%
    .[order(match(`F-method`, c("ARIMA", "ETS")),
            match(`Temp R-method`, c("None", "Var", "Struc")),
            match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] -> Summx
  
  Summx[, `F-method`] %>% unique() -> fmeth
  Summx[, `Test Set Length`] %>% unique() -> wind
  
  for (f in fmeth){
    for (w in wind){
    
    Summx[`F-method` == f & `Test Set Length` == w] %>%
      dcast(., `Temp R-method` + `CS R-method` + `F-method` + `Test Set Length` ~ `Series`,
            value.var = "1-4") %>%
      .[order(match(`F-method`, c("ARIMA", "ETS")),
              match(`Temp R-method`, c("None", "Var", "Struc")),
              match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] -> Summ.int
    
    Summ.int[`CS R-method` == "None" & `Temp R-method` == "None",
             !c("Temp R-method", "CS R-method", "F-method", "Test Set Length")] %>%
      as_vector() -> Base
    
    Summ.int[, !c("Temp R-method", "CS R-method", "F-method", "Test Set Length")] %>%
      as.matrix() -> Summ.mat
    
    matrix(c(NaN), nrow = nrow(Summ.mat), ncol = ncol(Summ.mat)) -> matx
    
    for (r in 1:nrow(Summ.mat)){
      
      (Summ.mat[r,]/Base-1)*100 -> matx[r,]
      
    }
    
    colnames(matx) <- colnames(Summ.mat)
    
    Summ.int[`CS R-method` == "None" & `Temp R-method` == "None",
             !c("Temp R-method", "CS R-method", "F-method", "Test Set Length")] %>%
      melt(., variable.name = "Series", value.name = "Base Error",
           measure.var = colnames(Summ.mat)) -> Base.Errors
    
    Summ.int[,.(`Temp R-method`, `CS R-method`, `F-method`, `Test Set Length`)] %>%
      cbind(., "Fc Term" = "1-4", matx) %>%
      melt(., id.vars = c("Temp R-method", "CS R-method", "F-method", "Test Set Length", "Fc Term"),
           variable.name = "Series",
           value.name = "MAPE") %>%
      .[Base.Errors, on = .(Series = Series)] %>%
      rbind(.dt, .) -> .dt
    
    }
  }
  
  # 5-8
  .data[(`Frequency` == "Quarterly" & `Fc Horizon` %in% 5:8) |
          (`Frequency` == "Biannual" & `Fc Horizon` %in% 3:4) |
          (`Frequency` == "Annual" & `Fc Horizon` == 2),
        .("5-8" = mean(AbsPerError)),
        by = .(`Series`, `Temp R-method`, `CS R-method`, `F-method`, `Test Set Length`)] %>%
    .[order(match(`F-method`, c("ARIMA", "ETS")),
            match(`Temp R-method`, c("None", "Var", "Struc")),
            match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] -> Summx
  
  Summx[, `F-method`] %>% unique() -> fmeth
  Summx[, `Test Set Length`] %>% unique() -> wind
  
  for (f in fmeth){
    for (w in wind){
    
    Summx[`F-method` == f & `Test Set Length` == w] %>%
      dcast(., `Temp R-method` + `CS R-method` + `F-method` + `Test Set Length` ~ `Series`,
            value.var = "5-8") %>%
      .[order(match(`F-method`, c("ARIMA", "ETS")),
              match(`Temp R-method`, c("None", "Var", "Struc")),
              match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] -> Summ.int
    
    Summ.int[`CS R-method` == "None" & `Temp R-method` == "None",
             !c("Temp R-method", "CS R-method", "F-method", "Test Set Length")] %>%
      as_vector() -> Base
    
    Summ.int[, !c("Temp R-method", "CS R-method", "F-method", "Test Set Length")] %>%
      as.matrix() -> Summ.mat
    
    matrix(c(NaN), nrow = nrow(Summ.mat), ncol = ncol(Summ.mat)) -> matx
    
    for (r in 1:nrow(Summ.mat)){
      
      (Summ.mat[r,]/Base-1)*100 -> matx[r,]
      
    }
    
    colnames(matx) <- colnames(Summ.mat)
    
    Summ.int[`CS R-method` == "None" & `Temp R-method` == "None",
             !c("Temp R-method", "CS R-method", "F-method", "Test Set Length")] %>%
      melt(., variable.name = "Series", value.name = "Base Error",
           measure.var = colnames(Summ.mat)) -> Base.Errors
    
    Summ.int[,.(`Temp R-method`, `CS R-method`, `F-method`, `Test Set Length`)] %>%
      cbind(., "Fc Term" = "5-8", matx) %>%
      melt(., id.vars = c("Temp R-method", "CS R-method", "F-method", "Test Set Length",  "Fc Term"),
           variable.name = "Series",
           value.name = "MAPE") %>%
      .[Base.Errors, on = .(Series = Series)] %>%
      rbind(.dt, .) -> .dt
    
    }
  }
  
  # 9-12
  .data[(`Frequency` == "Quarterly" & `Fc Horizon` %in% 9:12) |
          (`Frequency` == "Biannual" & `Fc Horizon` %in% 5:6) |
          (`Frequency` == "Annual" & `Fc Horizon` == 3),
        .("9-12" = mean(AbsPerError)),
        by = .(`Series`, `Temp R-method`, `CS R-method`, `F-method`, `Test Set Length`)] %>%
    .[order(match(`F-method`, c("ARIMA", "ETS")),
            match(`Temp R-method`, c("None", "Var", "Struc")),
            match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] -> Summx
  
  Summx[, `F-method`] %>% unique() -> fmeth
  Summx[, `Test Set Length`] %>% unique() -> wind
  
  for (f in fmeth){
    for (w in wind){
    
    Summx[`F-method` == f & `Test Set Length` == w] %>%
      dcast(., `Temp R-method` + `CS R-method` + `F-method` + `Test Set Length` ~ `Series`,
            value.var = "9-12") %>%
      .[order(match(`F-method`, c("ARIMA", "ETS")),
              match(`Temp R-method`, c("None", "Var", "Struc")),
              match(`CS R-method`, c("None", "Var", "MinT", "Var-A", "MinT-A")))] -> Summ.int
    
    Summ.int[`CS R-method` == "None" & `Temp R-method` == "None",
             !c("Temp R-method", "CS R-method", "F-method", "Test Set Length")] %>%
      as_vector() -> Base
    
    Summ.int[, !c("Temp R-method", "CS R-method", "F-method", "Test Set Length")] %>%
      as.matrix() -> Summ.mat
    
    matrix(c(NaN), nrow = nrow(Summ.mat), ncol = ncol(Summ.mat)) -> matx
    
    for (r in 1:nrow(Summ.mat)){
      
      (Summ.mat[r,]/Base-1)*100 -> matx[r,]
      
    }
    
    colnames(matx) <- colnames(Summ.mat)
    
    Summ.int[`CS R-method` == "None" & `Temp R-method` == "None",
             !c("Temp R-method", "CS R-method", "F-method", "Test Set Length")] %>%
      melt(., variable.name = "Series", value.name = "Base Error",
           measure.var = colnames(Summ.mat)) -> Base.Errors
    
    Summ.int[,.(`Temp R-method`, `CS R-method`, `F-method`, `Test Set Length`)] %>%
      cbind(., "Fc Term" = "9-12", matx) %>%
      melt(., id.vars = c("Temp R-method", "CS R-method", "F-method", "Test Set Length", "Fc Term"),
           variable.name = "Series",
           value.name = "MAPE") %>%
      .[Base.Errors, on = .(Series = Series)] %>%
      rbind(.dt, .) -> .dt
    
    
    }
  }
  
  return(.dt)
  
}


data.table("Series" = character(),
           "Temp R-method" = character(),
           "CS R-method" = character(),
           "F-method" = character(),
           "Test Set Length" = integer(),
           "Fc Term" = character(),
           "Base Error" = character(),
           "MAPE" = double()) -> Tb5

# Total
Summt[`Series` == "Total"] -> Summ
calc2(.data = Summ, .dt = Tb5) -> Tb5


# States
Summt[str_detect(Series, "^state/")] -> Summ
calc2(Summ, Tb5) -> Tb5


# Sex
Summt[str_detect(Series, "^sex/")] -> Summ
calc2(Summ, Tb5) -> Tb5


# Legal Status
Summt[str_detect(Series, "^leg/")] -> Summ
calc2(Summ, Tb5) -> Tb5


# Indigenous status
Summt[str_detect(Series, "^ind/")] -> Summ
calc2(Summ, Tb5) -> Tb5


## Two-way interactions
# State x Sex
copy(Summt) %>%
  .[str_detect(Series, "^state_sex")] %>%
  .[, c("Series") := .("State x Sex")] -> Summ

calc2(Summ, Tb5) -> Tb5


# State x Legal
copy(Summt) %>%
  .[str_detect(Series, "^state_sex")] %>%
  .[, c("Series") := .("State x Legal")] -> Summ

calc2(Summ, Tb5) -> Tb5

# State x Indigenous
copy(Summt) %>%
  .[str_detect(Series, "^state_ind")] %>%
  .[, c("Series") := .("State x Indigenous")] -> Summ

calc2(Summ, Tb5) -> Tb5

# Sex x Legal
copy(Summt) %>%
  .[str_detect(Series, "^sex_leg/")] %>%
  .[, c("Series") := .("Sex x Legal")] -> Summ

calc2(Summ, Tb5) -> Tb5

# Sex x Indigenous
copy(Summt) %>%
  .[str_detect(Series, "^sex_ind/")] %>%
  .[, c("Series") := .("Sex x Indigenous")] -> Summ

calc2(Summ, Tb5) -> Tb5

# Indigenous x Legal
copy(Summt) %>%
  .[str_detect(Series, "^ind_leg/")] %>%
  .[, c("Series") := .("Indigenous x Legal")] -> Summ

calc2(Summ, Tb5) -> Tb5

## Three-way interactions
# State x Sex x Legal
copy(Summt) %>%
  .[str_detect(Series, "^state_sex_leg/")] %>%
  .[, c("Series") := .("State x Sex x Legal")] -> Summ

calc2(Summ, Tb5) -> Tb5

# State x Sex x Indigenous
copy(Summt) %>%
  .[str_detect(Series, "^state_sex_ind/")] %>%
  .[, c("Series") := .("State x Sex x Indigenous")] -> Summ

calc2(Summ, Tb5) -> Tb5

# State x Legal x Indigenous
copy(Summt) %>%
  .[str_detect(Series, "^state_leg_ind/")] %>%
  .[, c("Series") := .("State x Legal x Indigenous")] -> Summ

calc2(Summ, Tb5) -> Tb5

# Sex x Legal x Indigenous
copy(Summt) %>%
  .[str_detect(Series, "^sex_leg_ind/")] %>%
  .[, c("Series") := .("Sex x Legal x Indigenous")] -> Summ

calc2(Summ, Tb5) -> Tb5

# Bottom-level: State x Sex x Legal x Indigenous
copy(Summt) %>%
  .[!str_detect(Series, "Total") & !str_detect(Series, "^state/") &
      !str_detect(Series, "^sex/") & !str_detect(Series, "^leg/") &
      !str_detect(Series, "^ind/") & !str_detect(Series, "^state_sex/") &
      !str_detect(Series, "^state_leg/") & !str_detect(Series, "^state_ind/") &
      !str_detect(Series, "^sex_leg/") & !str_detect(Series, "^sex_ind/") &
      !str_detect(Series, "^ind_leg/") & !str_detect(Series, "^state_sex_leg/") &
      !str_detect(Series, "^state_sex_ind/") & !str_detect(Series, "^state_leg_ind/") &
      !str_detect(Series, "^sex_leg_ind/")] %>%
  .[, c("Series") := .("State x Sex x Legal x Indigenous")] %>%
  .[is.finite(`AbsPerError`) & is.numeric(`AbsPerError`)] -> Summ

calc2(Summ, Tb5) -> Tb5

#### Tb5 Analysis ####
Tb5[, `Base Error` := as.numeric(`Base Error`)]
str(Tb5)

Tb5[`Series` == "Total" & `Fc Term` == "1-4",
    .(`Temp R-method`, `CS R-method`, `F-method`, `Test Set Length`, `MAPE`)] %>%
  dcast(., `Temp R-method` + `CS R-method` + `F-method` ~ `Test Set Length`,
        value.var = c("MAPE")) %>%
  .[order(match(`F-method`, c("ETS", "ARIMA")),
          match(`Temp R-method`, c("None", "Var", "Struc")),
          match(`CS R-method`, c("None", "Var", "Var-A", "MinT", "MinT-A")))] -> Tb5.14

Tb5[`Series` == "Total" & `Fc Term` == "5-8",
    .(`Temp R-method`, `CS R-method`, `F-method`, `Test Set Length`, `MAPE`)] %>%
  dcast(., `Temp R-method` + `CS R-method` + `F-method` ~ `Test Set Length`,
        value.var = c("MAPE")) %>%
  .[order(match(`F-method`, c("ETS", "ARIMA")),
          match(`Temp R-method`, c("None", "Var", "Struc")),
          match(`CS R-method`, c("None", "Var", "Var-A", "MinT", "MinT-A")))] -> Tb5.58

Tb5[`Series` == "Total" & `Fc Term` == "9-12",
    .(`Temp R-method`, `CS R-method`, `F-method`, `Test Set Length`, `MAPE`)] %>%
  dcast(., `Temp R-method` + `CS R-method` + `F-method` ~ `Test Set Length`,
        value.var = c("MAPE")) %>%
  .[order(match(`F-method`, c("ETS", "ARIMA")),
          match(`Temp R-method`, c("None", "Var", "Struc")),
          match(`CS R-method`, c("None", "Var", "Var-A", "MinT", "MinT-A")))] -> Tb5.912

knitr::kable(Tb5.14, digits=2, booktabs = T)
knitr::kable(Tb5.58, digits=2, booktabs = T)
knitr::kable(Tb5.912, digits=2, booktabs = T)

