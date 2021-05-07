### MAIN FUNCTION TO ASSEMBLE THE WATERSHED VALUE TABLES, 
### CREATE THE RF MODELS, AND VISUALIZE RESULTS  ### 

# 0. PRELIMS
rm(list = ls())
pacman::p_load(randomForest, caTools, lubridate, rfUtilities, ranger, hydroGOF,tidyr,
               rgdal,raster, plyr, dplyr , data.table, reshape2, varhandle, xts,caret,
               Hmisc, splitstackshape,ggplot2, caret, gridExtra,ggpubr,Rmisc,vip)
mainDir <- 'C:/StreamflowRF'
dataDir <- 'C:/StreamflowRF/data'
vtDir <- 'C:/StreamflowRF_Results/VT/'
predDir <- 'C:/StreamflowRF_Results/results/'
SW.list <- c('aarb_a', 'aarb_n', 'abrb_s','arb_b', 'arb_c', 'crb_a', 'crb_be', 
             'crb_bu', 'crb_d', 'crb_j','crb_m', 'crb_p', 'crb_s', 
             'crb_t', 'crb_u', 'mrb_s', 'prb_a', 'prb_b', 'prb_c', 
             'prb_p', 'prb_r') 
setwd(mainDir)

# 0. OBSERVED DATA GAPS 
setwd(dataDir)
obs <- read.csv('obs_98_16.csv')
names(obs)[2:length(obs)] <- paste0('shed_', names(obs)[2:length(obs)])
obs$date <- seq(from = as.Date("1998-01-01"), to = as.Date("2016-12-31"), by = 'day')
obs$year <- format(as.Date(obs$date, format="%m/%d/%Y"),"%Y") 
obs <- obs[-c(1:730),]

na_obs <- obs %>% 
  group_by(year) %>% 
  summarise_at(vars(starts_with("shed")), ~sum(is.na(.)))
names(na_obs) <- sub("shed_", "", names(na_obs))

obs_yr <- data.frame(ID=na_obs[,1], Days=rowMeans(na_obs[,-c(1, length(na_obs))]))
names(obs_yr) <- c('Year', 'Days')
p<-ggplot(data=obs_yr, aes(x=Year, y=Days)) + theme_bw()+
  geom_bar(stat="identity",width=0.75) +  ylab("Missing daily data (n)")+
  theme(text = element_text(size=18),
        axis.text.x = element_text(angle=90, hjust=1))
setwd(predDir)
ggsave(plot=p, filename=paste0('Figure_Gaps.png'),
       device='png', dpi=600, width = 7.5, height = 5, units='in')

n<-names(obs)[2:22]  
obs[,2:22] <- obs[,2:22]/1000  
gaps <- lapply(1:21, function(x) ggplot(obs, aes_string('date', n[[x]])) +geom_line()+theme_bw()+
         ggtitle( substring(n[[x]],6))+    labs(x ='Year',y=bquote('Observed Q ('*m^-3~s^-1*')'))) 
ggsave( multiplot(gaps[[1]], gaps[[2]],gaps[[3]], gaps[[4]], gaps[[5]],
                  gaps[[6]], gaps[[7]],gaps[[8]], gaps[[9]], gaps[[10]],
                  gaps[[11]], gaps[[12]],gaps[[13]], gaps[[14]], gaps[[15]],
                  gaps[[16]], gaps[[17]],gaps[[18]], gaps[[19]], gaps[[20]],
                  gaps[[21]],cols=3),filename='PaperFigure_Obsn.png',device='png',
        dpi=600, width = 8, height = 16, units='in' )

setwd(mainDir)

# 1. VALUETABLE ASSEMBLY (run once)
  source('R/groupVT.R')
  
  start_time <- Sys.time()
  
  #all
  all.VT <- groupVT('all')
  #basin
  aarb.VT <- groupVT('aarb')
  abrb.VT <- groupVT('abrb')
  arb.VT <- groupVT('arb')
  crb.VT <- groupVT('crb')
  mrb.VT <- groupVT('mrb')
  prb.VT <- groupVT('prb')
  #watersheds
  each.VT <- lapply(1:length(SW.list), function(x) 
    groupVT(SW.list[[x]]))
  #assign clusters from k-means/pca clustering
  source('R/pca.R')
  clstr <- PCA(each.VT,4)
  clstr
  pca1.VT <-  groupVT('pca1')
  pca2.VT <-  groupVT('pca2')
  pca3.VT <-  groupVT('pca3')
  pca4.VT <-  groupVT('pca4')
   # pca5.VT <-  groupVT('pca5')
  #  pca6.VT <-  groupVT('pca6')
   # pca7.VT <-  groupVT('pca7')
    
  end_time <- Sys.time()
  end_time - start_time


# 2. MODEL TRAINING 

#Open valuetables
all.VT <- read.csv("C:/StreamflowRF_Results/VT/all.csv")
pca1.VT <- read.csv("C:/StreamflowRF_Results/VT/pca1.csv")
pca2.VT <- read.csv("C:/StreamflowRF_Results/VT/pca2.csv")
pca3.VT <- read.csv("C:/StreamflowRF_Results/VT/pca3.csv")
pca4.VT <- read.csv("C:/StreamflowRF_Results/VT/pca4.csv")
aarb.VT <- read.csv("C:/StreamflowRF_Results/VT/aarb.csv")
abrb.VT <- read.csv("C:/StreamflowRF_Results/VT/abrb.csv")
arb.VT <- read.csv("C:/StreamflowRF_Results/VT/arb.csv")
crb.VT <- read.csv("C:/StreamflowRF_Results/VT/crb.csv")
mrb.VT <- read.csv("C:/StreamflowRF_Results/VT/mrb.csv")
prb.VT <- read.csv("C:/StreamflowRF_Results/VT/prb.csv")
each.VT <- lapply(SW.list, function(x) read.csv(paste0("C:/StreamflowRF_Results/VT/",x, '.csv')))


### Model testing (held-out data)
setwd(mainDir)
source('R/predTest_LOYO.R')
predDir <-"C:/StreamflowRF_Results/results/validation_loyo"
val_yr <- c(45,23,23,45,45,45,78,45,45,45,12,45,45,45,45,45,01,34,34,34,34)
rb <- c(1,1,2,3,3,4,4,4,4,4,4,4,4,4,4,5,6,6,6,6,6)

start_time <- Sys.time()
lapply(1:length(unique(all.VT$C.bname)), function(x) predTest(1, all.VT, unique(all.VT$C.bname)[[x]],
                                                              'all', predDir, val_yr[[x]],'mo'))
lapply(1:length(unique(pca1.VT$C.bname)), function(x) predTest(1, pca1.VT,unique(pca1.VT$C.bname)[[x]], 
                                                               'pca', predDir, val_yr[clstr==1] [[x]],'mo'))
lapply(1:length(unique(pca2.VT$C.bname)), function(x) predTest(1, pca2.VT,unique(pca2.VT$C.bname)[[x]], 
                                                               'pca', predDir, val_yr[clstr==2] [[x]],'mo'))
lapply(1:length(unique(pca3.VT$C.bname)), function(x) predTest(1, pca3.VT,unique(pca3.VT$C.bname)[[x]], 
                                                               'pca', predDir, val_yr[clstr==3] [[x]],'mo'))
lapply(1:length(unique(pca4.VT$C.bname)), function(x) predTest(1, pca4.VT,unique(pca4.VT$C.bname)[[x]], 
                                                               'pca', predDir, val_yr[clstr==4] [[x]],'mo'))
lapply(1:length(unique(aarb.VT$C.bname)), function(x) predTest(1, aarb.VT,unique(aarb.VT$C.bname)[[x]],
                                                               'basin', predDir,val_yr[rb==1] [[x]],'mo'))
lapply(1:length(unique(abrb.VT$C.bname)), function(x) predTest(1, abrb.VT,unique(abrb.VT$C.bname)[[x]],
                                                               'basin', predDir,val_yr[rb==2] [[x]],'mo'))
lapply(1:length(unique(arb.VT$C.bname)), function(x) predTest(1, arb.VT,unique(arb.VT$C.bname)[[x]],
                                                              'basin', predDir,val_yr[rb==3] [[x]],'mo'))
lapply(1:length(unique(crb.VT$C.bname)), function(x) predTest(1, crb.VT,unique(crb.VT$C.bname)[[x]],
                                                              'basin', predDir,val_yr[rb==4] [[x]],'mo'))
lapply(1:length(unique(mrb.VT$C.bname)), function(x) predTest(1, mrb.VT,unique(mrb.VT$C.bname)[[x]],
                                                             'basin', predDir,val_yr[rb==5] [[x]],'mo'))
lapply(1:length(unique(prb.VT$C.bname)), function(x)predTest(1, prb.VT,unique(prb.VT$C.bname)[[x]],
                                                             'basin', predDir,val_yr[rb==6] [[x]],'mo'))
lapply(1:21, function(x) predTest(1,each.VT[[x]], SW.list[[x]], 'shed',predDir, val_yr[[x]],'mo'))
end_time <- Sys.time()
end_time - start_time


### MODEL EVALUATIONS
setwd(mainDir)
source('R/Acc.R')
predDir <-"C:/StreamflowRF_Results/results/validation_loyo"
setwd(predDir)
pred.list <- list.files(predDir,pattern=glob2rx('*pca*1*csv'))
SW.list <-gsub("(_[a-z]).*","\\1",pred.list)
pca.df1 <-lapply(pred.list, function(x) read.csv(x))
pca.df1 <- lapply(pca.df1, transform, date = as.Date(date)) 
pca.df1 <- lapply(1:length(pca.df1), function(x) valYears(pca.df1[[x]],val_yr[[x]]))
pca.df1 <- lapply(pca.df1, function(x) { x$mo <- floor_date(x$date, "month");return(x)})
pca.df1 <- lapply(pca.df1, function(x) data.table(x))
pca.df1 <- lapply(pca.df1, function(x) x[, lapply(.SD, mean), by = mo])
pca.graph <- RegLine(ldply(pca.df1,data.frame), 'PCA-clustered')
pca.df <- ldply(lapply(1:length(pred.list), function(x) Acc(pca.df1[[x]],SW.list[[x]])), data.frame) 
pca.df$method <- 'PCA-clustered'

pred.list <- list.files(predDir,pattern=glob2rx('*basin*1*csv'))
SW.list <-gsub("(_[a-z]).*","\\1",pred.list)
basin.df1 <-lapply(pred.list, function(x) read.csv(x))
basin.df1 <- lapply(basin.df1, transform, date = as.Date(date)) 
basin.df1 <- lapply(1:length(basin.df1),  function(x) valYears(basin.df1[[x]],val_yr[[x]]))
basin.df1 <- lapply(basin.df1, function(x) { x$mo <- floor_date(x$date, "month");return(x)})
basin.df1 <- lapply(basin.df1, function(x) data.table(x))
basin.df1 <- lapply(basin.df1, function(x) x[, lapply(.SD, mean), by = mo])
basin.graph <- RegLine(ldply(basin.df1,data.frame), 'Basin-clustered')
basin.df <- ldply(lapply(1:length(pred.list), function(x) Acc(basin.df1[[x]],SW.list[[x]])), data.frame) 
basin.df$method <- 'Basin-clustered'
basin.graph[[2]]

pred.list <- list.files(predDir,pattern=glob2rx('*all*1*csv'))
SW.list <- c('aarb_a', 'aarb_n', 'abrb_s','arb_b', 'arb_c', 'crb_a', 'crb_be', 
             'crb_bu', 'crb_d', 'crb_j','crb_m', 'crb_p', 'crb_s', 
             'crb_t', 'crb_u', 'mrb_s', 'prb_a', 'prb_b', 'prb_c', 
             'prb_p', 'prb_r') 
all.df1 <- lapply(pred.list, function(x) read.csv(x))
all.df1 <- lapply(all.df1, transform, date = as.Date(date)) 
all.df1 <- lapply(1:length(all.df1),  function(x) valYears(all.df1[[x]],val_yr[[x]]))
all.df1 <- lapply(all.df1, function(x) { x$mo <- floor_date(x$date, "month");return(x)})
all.df1 <- lapply(all.df1, function(x) data.table(x))
all.df1 <- lapply(all.df1, function(x) x[, lapply(.SD, mean), by = mo])
all.graph <- RegLine(ldply(all.df1, data.frame),'One-clustered')
all.df <- ldply(lapply(1:length(pred.list), function(x) Acc(all.df1[[x]],SW.list[[x]])), data.frame) 
all.df$method <- 'One-clustered'
all.graph[[2]]

pred.list <- list.files(predDir,pattern=glob2rx('*shed*1*csv'))
shed.df1 <- lapply(pred.list, function(x) read.csv(x))
shed.df1 <- lapply(shed.df1, transform, date = as.Date(date)) 
shed.df1 <- lapply(1:length(shed.df1),  function(x) valYears(shed.df1[[x]],val_yr[[x]]))
shed.df1 <- lapply(shed.df1, function(x) { x$mo <- floor_date(x$date, "month");return(x)})
shed.df1 <- lapply(shed.df1, function(x) data.table(x))
shed.df1 <- lapply(shed.df1, function(x) x[, lapply(.SD, mean), by = mo])
shed.graph <-  RegLine(ldply(shed.df1,data.frame),   'Watershed-level')
shed.df <- ldply(lapply(1:length(pred.list), function(x) Acc(shed.df1[[x]],SW.list[[x]])), data.frame) 
shed.df$method <- 'Watershed-level'
shed.graph[[2]]

all.pred <- left_join(all.df, pca.df, by = c('str'='str'))
all.pred <- left_join(all.pred, basin.df, by = c('str'='str'))
all.pred <- data.frame(all.df, pca.df,basin.df,shed.df)
write.csv(all.pred, 'acc_final_bc_mo1.csv', row.names=F)
ggsave(plot=grid.arrange(pca.graph[[2]],all.graph[[2]],basin.graph[[2]],
                         shed.graph[[2]],nrow=2, ncol=2),
       filename='Q_loow_multi_bc_mo1.png',device='png', dpi=600, width = 12, height = 10, units='in' )       


#summary stat
all.pred <- rbind(all.df,pca.df,basin.df,shed.df)
all.pred$R1 <- ifelse(all.pred$NSE1 <0, all.pred$R, all.pred$R1)
all.pred$PBIAS1 <- ifelse(all.pred$NSE1 <0, all.pred$PBIAS, all.pred$PBIAS1)
all.pred$RMSE1 <- ifelse(all.pred$NSE1 <0, all.pred$RMSE, all.pred$RMSE1)
all.pred$NSE1 <- ifelse(all.pred$NSE1 <0, all.pred$NSE, all.pred$NSE1)

p1 <- ggplot(aes(y = NSE, x = method), data = all.pred) + geom_boxplot()+theme_bw()+
  labs(x = 'Regionalization method',y='NSE') + theme(text = element_text(size=22))#,axis.text.x = element_text(angle = 45))
p2 <- ggplot(aes(y = R, x = method), data = all.pred) + geom_boxplot()+theme_bw()+
  labs(x = 'Regionalization method',y=bquote(R^2)) + theme(text = element_text(size=22))
p3 <- ggplot(aes(y = PBIAS, x = method), data = all.pred) + geom_boxplot()+theme_bw()+
  labs(x = 'Regionalization method',y='PBIAS (%)') + theme(text = element_text(size=22))
p4 <- ggplot(aes(y = RMSE, x = method), data = all.pred) + geom_boxplot()+theme_bw()+
  labs(x = 'Regionalization method',y='R.RMSE (%)') + theme(text = element_text(size=22))

ggsave(plot=grid.arrange(p1,p2,p3,p4, nrow=2, ncol=2),
       filename='PaperFigure_SummaryStat_mo1.png',device='png', dpi=600, width = 18, height = 12, units='in' )


### Monthly aggregates graph
setwd(mainDir)
source('R/seasonal.R')





















## data frame one-time
all.df <- ldply(lapply(1:21, function(x) read.csv(paste0('C:/StreamflowRF/results/', 
                            SW.list[[x]],'_all_', calVal_ratio,'_pred.csv'))), data.frame)
all.df.bc <- ldply(lapply(1:21, function(x) read.csv(paste0('C:/StreamflowRF/results/validation_bc/', 
                                                         SW.list[[x]],'_all_', calVal_ratio,'_pred.csv'))), data.frame)
all.df.bc$predicted <- all.df$predicted - (all.df.bc$predicted - all.df$predicted)

all.df.trim <- ldply(lapply(1:21, function(x) read.csv(paste0('C:/StreamflowRF/results/validation_trim/', 
                                                            SW.list[[x]],'_all_', calVal_ratio,'_pred.csv'))), data.frame)


pca.df <- ldply(lapply(1:21, function(x) read.csv(paste0('M:/THESIS_PP/finalresults/predictions/', 
                                                         SW.list[[x]],'_pca_pred.csv'))), data.frame)

basin.df <- ldply(lapply(1:21, function(x) read.csv(paste0('M:/THESIS_PP/finalresults/predictions/', 
                                                         SW.list[[x]],'_basin_pred.csv'))), data.frame)

shed.df <- ldply(lapply(1:21, function(x) read.csv(paste0('M:/THESIS_PP/finalresults/predictions/', 
                                                         SW.list[[x]],'_shed_pred.csv'))), data.frame)

# ACC one-time
R2(all.df$observed, all.df$predicted)
R2(all.df.bc$observed, all.df.bc$predicted)
R2(all.df.trim$observed, all.df.trim$predicted)
NSeff(sim=all.df$predicted, obs=all.df$observed)
pbias(all.df$predicted, all.df$observed)


### GRAPHS
all.df$year <- format(as.Date(all.df$date, format="%Y-%m-%d"),"%Y")
pca.df$year <- format(as.Date(pca.df$date, format="%Y-%m-%d"),"%Y")
basin.df$year <- format(as.Date(basin.df$date, format="%Y-%m-%d"),"%Y")
shed.df$year <- format(as.Date(shed.df$date, format="%Y-%m-%d"),"%Y")

all.df1 <- subset(all.df, all.df$year == '2008' | all.df$year == '2008')
pca.df1 <- subset(pca.df, all.df$year == '2008' | pca.df$year == '2008')
basin.df1 <- subset(basin.df, all.df$year == '2008' | basin.df$year == '2008')
shed.df1 <- subset(shed.df, all.df$year == '2008' | shed.df$year == '2008')

g.all <- testAll(all.df,'Non-clustered')
g.pca <-testAll(pca.df,'Clustered')
g.basin <-testAll(basin.df,'Semi-clustered')
g.shed <-testAll(shed.df,'Non-regional')

Figure1 <- ggarrange(g.pca,g.basin,g.all,g.shed + 
                       font("x.text", size = 15),ncol = 1, nrow = 4, 
                     #labels = c( "Clustered","Semi-clustered", "Non-clustered", 'Non-regional'),
                     #  font.label = list(size = 32, color = "black"),
                     hjust = 0, vjust=5)


Figure2 <- ggarrange(g.pca,g.basin,g.all,g.shed + 
                     font("x.text", size = 15),ncol = 1, nrow = 4, 
                    # labels = c( "Clustered","Semi-clustered", "Non-clustered", 'Non-regional'),
                     font.label = list(size = 32, color = "black"),
                     hjust = -0.5, vjust=1)
Figure2



Figure3 <- ggarrange(Figure1, Figure2 + 
                       font("x.text", size = 15),ncol = 2, nrow = 1, 
                     # labels = c( "Clustered","Semi-clustered", "Non-clustered", 'Non-regional'),
                     font.label = list(size = 32, color = "black"),
                     hjust = -0.5, vjust=1)

setwd(plotDir)
ggsave(plot=Figure1, filename=paste0('PaperFigure_RegFin2.png'),
       device='png', dpi=300, width = 15, height = 30, units='in')
setwd(mydir)

