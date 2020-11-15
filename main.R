### MAIN FUNCTION TO ASSEMBLE THE WATERSHED VALUE TABLES, 
### CREATE THE RF MODELS, AND VISUALIZE RESULTS  ### 

# 0. PRELIMS
rm(list = ls())
pacman::p_load(caTools, lubridate, rfUtilities, ranger, hydroGOF,factoextra,corrplot,
               rgdal,raster, plyr, dplyr , data.table, reshape2, varhandle, xts,EcoHydRology,
               Hmisc, splitstackshape,ggplot2, ggpmisc, caret, gridExtra,ggpubr,Rmisc)
mainDir <- 'D:/StreamflowRF'
dataDir <- 'D:/StreamflowRF/data'
vtDir <- 'D:/StreamflowRF/intermediate/VT/'
predDir <- 'D:/StreamflowRF/results/'
plotDir <- 'D:/THESIS_PP/finalresults/plots/' #figureDir?
SW.list <- c('aarb_a', 'aarb_n', 'abrb_s','arb_b', 'arb_c', 'crb_a', 'crb_be', 
             'crb_bu', 'crb_d', 'crb_j','crb_m', 'crb_p', 'crb_s', 
             'crb_t', 'crb_u', 'mrb_s', 'prb_a', 'prb_b', 'prb_c', 
             'prb_p', 'prb_r') 
setwd(mainDir)


# 1. VALUETABLE ASSEMBLY
source('R/groupVT.R')

#all
all.VT.yes <- groupVT('all', 0.6, 'yes')

#basin
aarb.VT.yes <- groupVT('aarb', 0.6, 'yes')
abrb.VT.yes <- groupVT('abrb', 0.6, 'yes')
arb.VT.yes <- groupVT('arb', 0.6, 'yes')
crb.VT.yes <- groupVT('crb', 0.6, 'yes')
mrb.VT.yes <- groupVT('mrb', 0.6, 'yes')
prb.VT.yes <- groupVT('prb', 0.6, 'yes')

#watersheds
each.VT <- lapply(1:length(SW.list), function(x) groupVT(SW.list[[x]], 0.6, 'yes'))

#assign clusters from k-means/pca clustering
source('R/PCA.R')
clstr <- PCA(each.VT)
clstr
pca1.VT <-  groupVT('pca1', 0.6, 'yes')
pca2.VT <-  groupVT('pca2', 0.6, 'yes') 
pca3.VT <-  groupVT('pca3', 0.6, 'yes')
pca4.VT <-  groupVT('pca4', 0.6, 'yes')

# 2. RUNNING RF
all.RF <- ranger(all.VT.yes[[17]] ~ ., data=all.VT.yes[,-17], importance='permutation',
                 mtry=15, keep.inbag=T)

pca1.RF <- ranger(pca1.VT[[17]] ~ ., data=pca1.VT[,-17],mtry=15,  keep.inbag = T,importance='permutation')
pca2.RF <- ranger(pca2.VT[[17]] ~ ., data=pca2.VT[,-17],mtry=15,  keep.inbag = T,importance='permutation')
pca3.RF <- ranger(pca3.VT[[17]] ~ ., data=pca3.VT[,-17],mtry=15,  keep.inbag = T,importance='permutation')
pca4.RF <- ranger(pca4.VT[[17]] ~ ., data=pca4.VT[,-17],mtry=15,  keep.inbag = T,importance='permutation')

aarb.RF <- ranger(aarb.VT.yes[[17]] ~ ., data=aarb.VT.yes[,-17],mtry=15,  keep.inbag = T,importance='permutation')
abrb.RF <- ranger(abrb.VT.yes[[17]] ~ ., data=abrb.VT.yes[,-17],mtry=15, keep.inbag = T,importance='permutation')
arb.RF <- ranger(arb.VT.yes[[17]] ~ ., data=arb.VT.yes[,-17],mtry=15,keep.inbag = T,importance='permutation')
crb.RF <- ranger(crb.VT.yes[[17]] ~ ., data=crb.VT.yes[,-17],mtry=15,keep.inbag = T,importance='permutation')
mrb.RF <- ranger(mrb.VT.yes[[17]] ~ ., data=mrb.VT.yes[,-17],mtry=15, keep.inbag = T,importance='permutation')
prb.RF <- ranger(prb.VT.yes[[17]] ~ ., data=prb.VT.yes[,-17],mtry=15,keep.inbag = T,importance='permutation')

each.RF <-  lapply(1:length(SW.list), function(x) ranger(each.VT[[x]][[17]] ~ ., data=each.VT[[x]][,-17],
                                                         mtry=15, keep.inbag = T))

### MODEL TESTS
pca.models<- list(pca4.RF, pca4.RF, pca4.RF, pca4.RF, pca4.RF, pca4.RF, pca1.RF, pca3.RF,
                  pca1.RF, pca4.RF, pca1.RF, pca4.RF, pca4.RF, pca4.RF, pca3.RF, pca1.RF, 
                  pca2.RF, pca4.RF, pca4.RF, pca4.RF, pca4.RF)
rm(pca1.RF, pca1.RF, pca1.RF, pca1.RF, pca1.RF, pca1.RF, pca3.RF, pca4.RF,
  pca3.RF, pca1.RF, pca3.RF, pca1.RF, pca1.RF, pca1.RF, pca4.RF, pca3.RF, 
  pca2.RF, pca1.RF, pca1.RF, pca1.RF, pca1.RF)

basin.models <- list(aarb.RF, aarb.RF, abrb.RF, arb.RF, arb.RF, crb.RF, crb.RF,
                     crb.RF,crb.RF,crb.RF,crb.RF,crb.RF,crb.RF,crb.RF,crb.RF,
                     mrb.RF, prb.RF,prb.RF,prb.RF,prb.RF,prb.RF)
rm(aarb.RF, aarb.RF, abrb.RF, arb.RF, arb.RF, crb.RF, crb.RF,
  crb.RF,crb.RF,crb.RF,crb.RF,crb.RF,crb.RF,crb.RF,crb.RF,
  mrb.RF, prb.RF,prb.RF,prb.RF,prb.RF,prb.RF)

### Model testing (held-out data)
set.seed(123)
source('R/predTest.R')
cv.all <- lapply(1:21, function(x) predTest(0.60, SW.list[[x]], all.RF, 'all'))
cv.pca <- lapply(1:21, function(x) predTest(0.60, SW.list[[x]], pca.models[[x]], 'pca'))
cv.basin <- lapply(1:21, function(x) predTest(0.60, SW.list[[x]], basin.models[[x]], 'basin'))
cv.shed <- lapply(1:21, function(x) predTest(0.60, SW.list[[x]], each.RF[[x]], 'shed'))

# Indpendent watershed validation (crb_p or Pinacanauan de-Ilagan basin) #12 of SW_list
each.VT <- lapply(1:length(SW.list), function(x) groupVT(SW.list[[x]], 1, 'yes'))
crb_p <- each.VT[[12]]
crb_p.all <- predTest(1, SW.list[[12]], all.RF, 'all')
crb_p.pca <- predTest(1, SW.list[[12]], pca.models[[12]], 'pca')
crb_p.basin <- predTest(1, SW.list[[12]], basin.models[[12]], 'basin')
crb_p.shed <- predTest(1, SW.list[[12]], each.RF[[12]], 'shed')

crb_p.pca$mo  <- months.Date(crb_p.pca$date)
crb_p.pca$year  <- year(crb_p.pca$date)
crb_p.pca <- subset(crb_p.pca, crb_p.pca$year > 2002)
crb_p.pca <- aggregate( predicted ~ mo + year  , crb_p.pca , mean )
crb_p.pca$year1 <- as.yearmon(paste0(crb_p.pca$year,'-', crb_p.pca$mo))

crb_p.pca <- crb_p.pca[with(crb_p.pca, order(mo,year)), ]

abuan <- read.csv('D:/ABUAN_SWAT/cal_val5.csv')
abuan$year <- as.yearmon(paste0(abuan$Year,'-', abuan$Month))
abuan <- abuan[,c('year', 'pcp', 'uncalibrated', 'calibrated', 'observed')]
abuan$mo <- months.Date(abuan$year)
abuan <- abuan[with(abuan, order(mo,year)), ]
abuan$predicted <- crb_p.pca$predicted
abuan <- abuan[with(abuan, order(year)), ]

hydrograph(timeSeries = abuan$year, streamflow=abuan$observed,precip = abuan$pcp,
           streamflow2=abuan$predicted,streamflow3=abuan$calibrated, S.units='m3s', P.units = 'mm')



### ACCURACY
source('R/Acc.R')
all.df <- ldply(lapply(1:21, function(x) Acc(cv.all[[x]],SW.list[[x]])), data.frame)
all.df$method <- 'all'
pca.df <- ldply(lapply(1:21, function(x) Acc(read.csv(paste0(getwd(), '/results/', 
                                                         SW.list[[x]],'_pca_pred.csv')),SW.list[[x]])), data.frame)
pca.df$method <- 'pca'
basin.df <- ldply(lapply(1:21, function(x) Acc(read.csv(paste0(getwd(), '/results/', 
                                                           SW.list[[x]],'_basin_pred.csv')),SW.list[[x]])),data.frame)
basin.df$method <- 'basin'
shed.df <- ldply(lapply(1:21, function(x) Acc(read.csv(paste0(getwd(), '/results/', 
                                                          SW.list[[x]],'_shed_pred.csv')),SW.list[[x]])),data.frame)
shed.df$method <- 'shed'

accAll <- cbind(pca.df,basin.df[-1],all.df[-1],shed.df[-1])
setwd(paste0(getwd(), '/results'))
write.csv(accAll, 'accAll_40test.csv', row.names=F)


## data frame one-time
all.df <- ldply(lapply(1:21, function(x) read.csv(paste0('M:/THESIS_PP/finalresults/predictions/', 
                            SW.list[[x]],'_all_pred.csv'))), data.frame) ###########!!!!!!!!!!!!!!!!
pca.df <- ldply(lapply(1:21, function(x) read.csv(paste0('M:/THESIS_PP/finalresults/predictions/', 
                                                         SW.list[[x]],'_pca_pred.csv'))), data.frame)

basin.df <- ldply(lapply(1:21, function(x) read.csv(paste0('M:/THESIS_PP/finalresults/predictions/', 
                                                         SW.list[[x]],'_basin_pred.csv'))), data.frame)

shed.df <- ldply(lapply(1:21, function(x) read.csv(paste0('M:/THESIS_PP/finalresults/predictions/', 
                                                         SW.list[[x]],'_shed_pred.csv'))), data.frame)

# ACC one-time
R2(all.df$observed, all.df$predicted)
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

