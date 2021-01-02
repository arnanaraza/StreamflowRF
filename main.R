### MAIN FUNCTION TO ASSEMBLE THE WATERSHED VALUE TABLES, 
### CREATE THE RF MODELS, AND VISUALIZE RESULTS  ### 

# 0. PRELIMS
rm(list = ls())
pacman::p_load(caTools, lubridate, rfUtilities, ranger, hydroGOF,factoextra,corrplot,
               rgdal,raster, plyr, dplyr , data.table, reshape2, varhandle, xts,EcoHydRology,
               Hmisc, splitstackshape,ggplot2, ggpmisc, caret, gridExtra,ggpubr,Rmisc)
mainDir <- 'D:/StreamflowRF'
dataDir <- 'D:/StreamflowRF/data'
vtDir <- 'D:/StreamflowRF_Results/intermediate/VT/'
predDir <- 'D:/StreamflowRF_Results/results/'
SW.list <- c('aarb_a', 'aarb_n', 'abrb_s','arb_b', 'arb_c', 'crb_a', 'crb_be', 
             'crb_bu', 'crb_d', 'crb_j','crb_m', 'crb_p', 'crb_s', 
             'crb_t', 'crb_u', 'mrb_s', 'prb_a', 'prb_b', 'prb_c', 
             'prb_p', 'prb_r') 
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
  source('R/PCA.R')
  clstr <- PCA(each.VT,4)
  clstr
  pca1.VT <-  groupVT('pca1')
  pca2.VT <-  groupVT('pca2')
  pca3.VT <-  groupVT('pca3')
  pca4.VT <-  groupVT('pca4')
  
  end_time <- Sys.time()
  end_time - start_time


# 2. MODEL TRAINING 

#Open valuetables
all.VT <- read.csv("D:/StreamflowRF_Results/intermediate/VT/all.csv")
pca1.VT <- read.csv("D:/StreamflowRF_Results/intermediate/VT/pca1.csv")
pca2.VT <- read.csv("D:/StreamflowRF_Results/intermediate/VT/pca2.csv")
pca3.VT <- read.csv("D:/StreamflowRF_Results/intermediate/VT/pca3.csv")
pca4.VT <- read.csv("D:/StreamflowRF_Results/intermediate/VT/pca4.csv")
aarb.VT <- read.csv("D:/StreamflowRF_Results/intermediate/VT/aarb.csv")
abrb.VT <- read.csv("D:/StreamflowRF_Results/intermediate/VT/abrb.csv")
arb.VT <- read.csv("D:/StreamflowRF_Results/intermediate/VT/arb.csv")
crb.VT <- read.csv("D:/StreamflowRF_Results/intermediate/VT/crb.csv")
mrb.VT <- read.csv("D:/StreamflowRF_Results/intermediate/VT/mrb.csv")
prb.VT <- read.csv("D:/StreamflowRF_Results/intermediate/VT/prb.csv")
each.VT <- lapply(SW.list, function(x) read.csv(paste0("D:/StreamflowRF_Results/intermediate/VT/",x, '.csv')))


### Model testing (held-out data)
setwd(mainDir)
source('R/predTest.R')
predDir <-"D:/StreamflowRF_Results/results/validation_bc40"
#predDir <-"D:/StreamflowRF_Results/results/validation_LOOCV"

start_time <- Sys.time()
lapply(unique(all.VT$C.bname), function(x) predTest(0.4, all.VT, x, 'all', predDir))
lapply(unique(pca1.VT$C.bname), function(x) predTest(0.4, pca1.VT,x, 'pca', predDir))
lapply(unique(pca2.VT$C.bname), function(x) predTest(0.4, pca2.VT,x, 'pca', predDir))
lapply(unique(pca3.VT$C.bname), function(x) predTest(0.4, pca3.VT,x, 'pca', predDir))
lapply(unique(pca4.VT$C.bname), function(x) predTest(0.4, pca4.VT,x, 'pca', predDir))
lapply(unique(aarb.VT$C.bname), function(x) predTest(0.4, aarb.VT,x, 'basin', predDir))
lapply(unique(abrb.VT$C.bname), function(x) predTest(0.4, abrb.VT,x, 'basin', predDir))
lapply(unique(arb.VT$C.bname), function(x) predTest(0.4, arb.VT,x, 'basin', predDir))
lapply(unique(crb.VT$C.bname), function(x) predTest(0.4, crb.VT,x, 'basin', predDir))
lapply(unique(mrb.VT$C.bname), function(x) predTest(0.4, mrb.VT,x, 'basin', predDir))
lapply(unique(prb.VT$C.bname), function(x) predTest(0.4, prb.VT,x, 'basin', predDir))
lapply(1:21, function(x) predTest(0.4,each.VT[[x]], SW.list[[x]], 'shed',predDir))
end_time <- Sys.time()
end_time - start_time


### ACCURACY
setwd(mainDir)
source('R/Acc.R')
setwd(predDir)
pred.list <- list.files(predDir,pattern=glob2rx('*all*0.4*'))
all.df <- ldply(lapply(1:length(pred.list), function(x) Acc(read.csv(pred.list[[x]]),SW.list[[x]])), data.frame) 
all.df$method <- 'all'
  
pred.list <- list.files(predDir,pattern=glob2rx('*pca*0.4*'))
SW.list <-gsub("(_[a-z]).*","\\1",pred.list)
SW.list[7:8] <- c('crb_be', 'crb_bu')
pca.df <- ldply(lapply(1:length(pred.list), function(x) Acc(read.csv(pred.list[[x]]),SW.list[[x]])), data.frame) 
pca.df$method <- 'pca'

pred.list <- list.files(predDir,pattern=glob2rx('*basin*0.4*'))
SW.list <-gsub("(_[a-z]).*","\\1",pred.list)
SW.list[6:7] <- c('crb_be', 'crb_bu')
basin.df <- ldply(lapply(1:length(pred.list), function(x) Acc(read.csv(pred.list[[x]]),SW.list[[x]])), data.frame) 
basin.df$method <- 'basin'

pred.list <- list.files(predDir,pattern=glob2rx('*shed*0.4*'))
SW.list <-gsub("(_[a-z]).*","\\1",pred.list)
shed.df <- ldply(lapply(1:length(pred.list), function(x) Acc(read.csv(pred.list[[x]]),SW.list[[x]])), data.frame) 
shed.df$method <- 'shed'


all.pred <- left_join(all.df, pca.df, by = c('str'='str'))
all.pred <- left_join(all.pred, basin.df, by = c('str'='str'))
all.pred <- data.frame(all.df, pca.df,basin.df, shed.df)
write.csv(all.pred, 'acc_40_bc.csv', row.names=F)


## data frame one-time
all.df <- ldply(lapply(1:21, function(x) read.csv(paste0('D:/StreamflowRF/results/', 
                            SW.list[[x]],'_all_', calVal_ratio,'_pred.csv'))), data.frame)
all.df.bc <- ldply(lapply(1:21, function(x) read.csv(paste0('D:/StreamflowRF/results/validation_bc/', 
                                                         SW.list[[x]],'_all_', calVal_ratio,'_pred.csv'))), data.frame)
all.df.bc$predicted <- all.df$predicted - (all.df.bc$predicted - all.df$predicted)

all.df.trim <- ldply(lapply(1:21, function(x) read.csv(paste0('D:/StreamflowRF/results/validation_trim/', 
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

