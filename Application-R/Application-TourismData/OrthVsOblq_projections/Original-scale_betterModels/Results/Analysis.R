
library(tidyverse)

library(xtable)


# OvernightTrips_OriginalScale_Fc_1_50 <- read.csv("DF_OriginalScale_1-50.csv")[,-1]
# OvernightTrips_OriginalScale_Fc_51_100 <- read.csv("DF_OriginalScale_51-100.csv")[,-1]
# OvernightTrips_OriginalScale_Fc_101_140 <- read.csv("DF_OriginalScale_101-140.csv")[,-1]
# 
# rbind(OvernightTrips_OriginalScale_Fc_1_50, OvernightTrips_OriginalScale_Fc_51_100, 
#       OvernightTrips_OriginalScale_Fc_101_140) %>% 
#   as_tibble() -> OvernightTrips_OriginalScale_Fc
# write.csv(x=OvernightTrips_OriginalScale_Fc, file = "OvernightTrips_OriginalScale_Fc.csv")

OvernightTrips_OriginalScale_Fc <- read.csv("DF_OriginalScale_all.csv")[,-1]
OvernightTrips_OriginalScale_Fc %>% 
  as_tibble() -> OvernightTrips_OriginalScale_Fc
write.csv(x=OvernightTrips_OriginalScale_Fc, file = "OvernightTrips_OriginalScale_Fc.csv")

OvernightTrips_OriginalScale_Fc %>% 
  mutate(SquaredE = (Overnight_Trips - Overnight_Trips_Fc)^2) -> OvernightTrips_OriginalScale_Fc

# OvernightTrips_OriginalScale_Fc %>% 
#   group_by(`R.method`, Fc_horizon) %>% 
#   summarise(MSE = mean(SquaredE)) %>% 
#  summarise(MTSE=mean(TSE,na.rm = T),MWSE=mean(WSE,na.rm = T),MSpWSE=mean(SpWSE,na.rm = T))%>%View
#   spread(key = Fc_horizon, value = MSE)
# 
# 
# OvernightTrips_OriginalScale_Fc <- read.csv("OvernightTrips_OriginalScale_Fc.csv")[,-1]



OvernightTrips_OriginalScale_Fc %>%
  left_join(.,read_csv('weights.csv'))->OvernightTrips_OriginalScale_FcW


OvernightTrips_OriginalScale_FcW %>%
  mutate(SquaredE = (Overnight_Trips - Overnight_Trips_Fc)^2,
         WeightedSquaredE = (1/Weight^2)*((Overnight_Trips - Overnight_Trips_Fc)^2),
         SpWeightedSquaredE = (1/Spend^2)*((Overnight_Trips - Overnight_Trips_Fc)^2),
         Fc_horizon = recode(Fc_horizon, "1" = "h=1", "2" = "h=2",
                             "3" = "h=3", "4" = "h=4", "5" = "h=5", "6" = "h=6")) %>%
  group_by(`R.method`, Fc_horizon, Replication) %>%
  summarise(TSE = sum(SquaredE),`Structural-WSE` = sum(WeightedSquaredE),`Spend-WSE` = sum(SpWeightedSquaredE)) -> OvernightTrips_OriginalScale_MSE


#OvernightTrips_OriginalScale_MSE%>%
#  filter(Fc_horizon=='h=1')%>%
#  group_by(R.method)%>%


############Create Table (Messy)

for (j in 1:6){

hor<-paste0('h=',j)

OvernightTrips_OriginalScale_MSE %>%
  ungroup()%>%
  filter(Fc_horizon==hor)%>%
  select(-Fc_horizon)%>%
  group_by(R.method)%>%
  summarise(MTSE=mean(TSE,na.rm=T),MStrWSE=mean(`Structural-WSE`,na.rm=T),MSpeWSE=mean(`Spend-WSE`,na.rm=T))%>%
  filter(R.method%in%c('Base','Bottom-up','OLS','WLS1','WLS2','MinT(Shrink)'))->IntStep

methods<-pull(IntStep,R.method)
lfs<-colnames(IntStep)
IntStep%>%  
  select(-R.method)%>%
  t%>%
  as_tibble()->NumOnly
colnames(NumOnly)<-methods
add_column(NumOnly,`Loss Function`=c('TSE','Structural WSE','Spend WSE'),.before = 1)->NumOnly

NumOnly%>%
  transmute(`Loss Function`,
            `Bottom-up`=`Bottom-up`/Base,
            OLS=OLS/Base,
            `Structural-WLS`=WLS1/Base,
            `Spend-WLS`=WLS2/Base,
            MinT=`MinT(Shrink)`/Base)%>%
  add_column(Base=1,.before = 2)%>%
  xtable(caption = paste0('Means of different loss functions for ',j ,'-step ahead forecasts using different reconciliation methods in the tourism application.  All figures are reported relative to base forecasts.'))%>%
  print(include.rownames=F,file=paste0('lossvmethods_h_',j,'.tex'))


#Create boxplot 


OvernightTrips_OriginalScale_MSE %>%
  ungroup%>%
  #filter(`R.method`%in% c("Base", "OLS", "MinT(Shrink)", "WLS")) %>%
  filter(Fc_horizon==hor)%>%
  select(-Fc_horizon)%>%
  pivot_wider(id_cols = Replication,names_from = `R.method`,values_from = c('TSE',`Structural-WSE`,`Spend-WSE`))%>%
    mutate("TSE_OLS" = 1/(TSE_Base / TSE_OLS),
         "TSE_MinT" = 1/(TSE_Base/  `TSE_MinT(Shrink)`),
         "TSE_Structural-WLS" = 1/(TSE_Base / TSE_WLS1),
         "TSE_Spend-WLS" = 1/(TSE_Base / TSE_WLS2),
         "Structural-WSE_OLS" = 1/(`Structural-WSE_Base` / `Structural-WSE_OLS`),
         "Structural-WSE_MinT" = 1/(`Structural-WSE_Base` / `Structural-WSE_MinT(Shrink)`),
         "Structural-WSE_Structural-WLS" = 1/(`Structural-WSE_Base` / `Structural-WSE_WLS1`),
         "Structural-WSE_Spend-WLS" = 1/(`Structural-WSE_Base` / `Structural-WSE_WLS2`),
         "Spend-WSE_OLS" = 1/(`Spend-WSE_Base` / `Spend-WSE_OLS`),
         "Spend-WSE_MinT" = 1/(`Spend-WSE_Base` / `Spend-WSE_MinT(Shrink)`),
         "Spend-WSE_Structural-WLS" = 1/(`Spend-WSE_Base` / `Spend-WSE_WLS1`),
         "Spend-WSE_Spend-WLS" = 1/(`Spend-WSE_Base` / `Spend-WSE_WLS2`)) %>%
  select(Replication,`TSE_OLS`,`TSE_MinT`,`TSE_Structural-WLS`,`TSE_Spend-WLS`,
         `Structural-WSE_OLS`,`Structural-WSE_MinT`,`Structural-WSE_Structural-WLS`,`Structural-WSE_Spend-WLS`,
         `Spend-WSE_OLS`,`Spend-WSE_MinT`,`Spend-WSE_Structural-WLS`,`Spend-WSE_Spend-WLS`)%>%
  pivot_longer(-Replication,names_to=c('Loss Function','Method'),names_sep='_',values_to = 'Loss')%>%
  mutate('Loss Function'=factor(`Loss Function`,levels=c('TSE','Structural-WSE','Spend-WSE')))%>%
  mutate('Method'=factor(Method,levels=c('OLS','Structural-WLS','Spend-WLS','MinT')))%>%
  ggplot(aes(x=Method,y=Loss))+geom_boxplot()+facet_wrap(~`Loss Function`,ncol = 1,scales = 'free_y')
  ggsave(file=paste0('lossboxplots_',j,'.pdf'))


}
  

# #Create boxplot (TSE)
# 
# 
# OvernightTrips_OriginalScale_MSE %>%
#   ungroup%>%
#   #filter(`R.method`%in% c("Base", "OLS", "MinT(Shrink)", "WLS")) %>%
#   filter(Fc_horizon=='h=1')%>%
#   select(-`Spend-WSE`,-TSE)%>%
#   spread(key = `R.method`, value = `Structural-WSE`) %>%
#   mutate("OLS" = Base - OLS,
#          "MinT" = Base - `MinT(Shrink)`,
#          "Structural-WLS" = Base - WLS1,
#          "Spend-WLS" = Base - WLS2,
#          "Base-GMinT1" = Base - GMinT1,
#          "Base-GMinT2" = Base - GMinT2) %>%
#   select(Replication,`OLS`,`MinT`,`Structural-WLS`,`Spend-WLS`)%>%
#   pivot_longer(-Replication,names_to = "Method",values_to = "Structural-WSE")%>%
#   ggplot(aes(x=Method,y=`Structural-WSE`))+geom_boxplot()
# 
# #Create boxplot (SpWSE)
# 
# 
# OvernightTrips_OriginalScale_MSE %>%
#   ungroup%>%
#   #filter(`R.method`%in% c("Base", "OLS", "MinT(Shrink)", "WLS")) %>%
#   filter(Fc_horizon=='h=1')%>%
#   select(-TSE,-WSE)%>%
#   spread(key = `R.method`, value = SpWSE) %>%
#   mutate("Base-OLS" = Base - OLS,
#          "Base-MinT" = Base - `MinT(Shrink)`,
#          "Base-WLS1" = Base - WLS1,
#          "Base-WLS2" = Base - WLS2,
#          "Base-GMinT1" = Base - GMinT1,
#          "Base-GMinT2" = Base - GMinT2) %>%
#   select(Replication,`Base-OLS`,`Base-MinT`,`Base-WLS1`,`Base-WLS2`,`Base-GMinT1`,`Base-GMinT2`)%>%
#   pivot_longer(-Replication,names_to = "Method",values_to = "SpWSE")%>%
#   ggplot(aes(x=Method,y=SpWSE))+geom_boxplot()


# OvernightTrips_OriginalScale_Fc %>% 
#   filter(Fc_horizon == 1, Replication == 15) %>% 
#   summarise(Diff = )
  
