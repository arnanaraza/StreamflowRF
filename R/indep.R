# Indpendent watershed validation (crb_p or Pinacanauan de-Ilagan basin) #12 of SW_list
crb_p.VT <- groupVT(SW.list[[12]], 0.1, 'yes')
crb_p.RF <-  ranger(O.obs ~ ., data=crb_p.VT, mtry=15, keep.inbag = T)
crb_p.VT$O.obs <- crb_p.RF$predictions - (crb_p.VT$O.obs - crb_p.RF$predictions)
crb_p.RF1 <-  ranger(O.obs ~ ., data=crb_p.VT, mtry=15, keep.inbag = T)

  crb_p.all <- predTest(1, SW.list[[12]], all.RF, 'all')
  crb_p.all.bc <- predTest(1, SW.list[[12]], all.RF1, 'all')
  crb_p.all$predicted <- crb_p.all$predicted - (crb_p.all.bc$predicted - crb_p.all$predicted)
  
  crb_p.pca <- predTest(1, SW.list[[12]], pca.models[[12]], 'pca')
  crb_p.pca.bc <- predTest(1, SW.list[[12]], pca.models.bc[[12]], 'pca')
  crb_p.pca$predicted <- crb_p.pca$predicted - (crb_p.pca.bc$predicted - crb_p.pca$predicted)
  
  crb_p.basin <- predTest(1, SW.list[[12]], crb.RF, 'basin')
  crb_p.basin.bc <- predTest(1, SW.list[[12]], crb.RF1, 'basin')
  crb_p.basin$predicted <- crb_p.basin$predicted - (crb_p.basin.bc$predicted - crb_p.basin$predicted)
  
  crb_p.shed <- predTest(1, SW.list[[12]], crb_p.RF, 'shed')
  crb_p.shed.bc <- predTest(1, SW.list[[12]], crb_p.RF1, 'shed')
  crb_p.shed$predicted <- crb_p.shed$predicted - (crb_p.shed.bc$predicted - crb_p.shed$predicted)
  
  crb_p.pca=crb_p.basin

crb_p.pca$mo  <- months.Date(crb_p.pca$date)
crb_p.pca$year  <- year(crb_p.pca$date)
crb_p.pca <- subset(crb_p.pca, crb_p.pca$year > 2002)
crb_p.pca <- aggregate( cbind(observed,predicted) ~ mo + year  , crb_p.pca , mean )
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
NSE(abuan$predicted, abuan$observed)
NSE(abuan$calibrated, abuan$observed)


####Upper Marikina
mrb_s.VT <- groupVT(SW.list[[16]], 0.2, 'yes')
mrb_s.RF <-  ranger(mrb_s.VT[[17]] ~ ., data=mrb_s.VT[,-17],
                    mtry=15, keep.inbag = T)
mrb_s.all <- predTest(1, SW.list[[16]], all.RF, 'all')
mrb_s.pca <- predTest(1, SW.list[[16]], pca.models[[16]], 'pca')
mrb_s.basin <- predTest(1, SW.list[[16]], basin.models[[16]], 'basin')
mrb_s.shed <- predTest(1, SW.list[[16]], mrb_s.RF, 'shed')
mrb_s.pca=mrb_s.shed

mrb_s.pca$mo  <- months.Date(mrb_s.pca$date)
mrb_s.pca$year  <- year(mrb_s.pca$date)
mrb_s.pca <- subset(mrb_s.pca, mrb_s.pca$year > 2001)
mrb_s.pca <- aggregate( cbind(observed,predicted) ~ mo + year  , mrb_s.pca , mean )
mrb_s.pca$year1 <- as.yearmon(paste0(mrb_s.pca$year,'-', mrb_s.pca$mo))
mrb_s.pca <- mrb_s.pca[with(mrb_s.pca, order(mo,year)), ]

abuan <- read.csv(paste0(mainDir,'/data/val_mrb_s.csv'))
abuan <- subset(abuan, abuan$year < 2006)
abuan <- aggregate( cbind(pcp,calibrated) ~ mo + year, abuan , mean )
abuan <- abuan[with(abuan, order(mo,year)), ]

mrb_s.pca$calibrated <- abuan$calibrated
mrb_s.pca$pcp <- abuan$pcp
mrb_s.pca$mo =ifelse(mrb_s.pca$mo  == 'January', 1, mrb_s.pca$mo)
mrb_s.pca$mo =ifelse(mrb_s.pca$mo  == 'February', 2, mrb_s.pca$mo)
mrb_s.pca$mo =ifelse(mrb_s.pca$mo  == 'March', 3, mrb_s.pca$mo)
mrb_s.pca$mo =ifelse(mrb_s.pca$mo  == 'April', 4, mrb_s.pca$mo)
mrb_s.pca$mo =ifelse(mrb_s.pca$mo  == 'May', 5, mrb_s.pca$mo)
mrb_s.pca$mo =ifelse(mrb_s.pca$mo  == 'June', 6, mrb_s.pca$mo)
mrb_s.pca$mo =ifelse(mrb_s.pca$mo  == 'July', 7, mrb_s.pca$mo)
mrb_s.pca$mo =ifelse(mrb_s.pca$mo  == 'August', 8, mrb_s.pca$mo)
mrb_s.pca$mo =ifelse(mrb_s.pca$mo  == 'September', 9, mrb_s.pca$mo)
mrb_s.pca$mo =ifelse(mrb_s.pca$mo  == 'October', 10, mrb_s.pca$mo)
mrb_s.pca$mo =ifelse(mrb_s.pca$mo  == 'November', 11, mrb_s.pca$mo)
mrb_s.pca$mo =ifelse(mrb_s.pca$mo  == 'December', 12, mrb_s.pca$mo)
mrb_s.pca$mo =as.numeric(mrb_s.pca$mo )
mrb_s.pca <- mrb_s.pca[with(mrb_s.pca, order(mo,year)), ]

hydrograph(timeSeries = mrb_s.pca$year, streamflow=mrb_s.pca$observed,precip = mrb_s.pca$pcp,
           streamflow2=mrb_s.pca$predicted,streamflow3=mrb_s.pca$calibrated, S.units='m3s', P.units = 'mm')
NSE(mrb_s.pca$predicted, mrb_s.pca$observed)
NSE(mrb_s.pca$calibrated, mrb_s.pca$observed)

