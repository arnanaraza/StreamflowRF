
# Function to predict using RF model then 
predTest <- function (td=0.6, df=mrb.VT, basin='mrb_s', RFS='all',predDir,year) {
  dir.create(predDir, showWarnings = FALSE)
  
  #prepare training-test data 
  set.seed(123)
  if (td < 1 & td != 0.5){
    train.b <- df
    samp <- floor((1-td)*nrow(train.b))
    train_ind <- sample(seq_len(nrow(train.b)), size = samp)
    train <- train.b[-train_ind,] #
    train <- train[ , !(names(train) %in% c('date','C.bname'))]
    train[] <- sapply(train, as.numeric)
    
    #test data
      train.b <- read.csv(paste0(mainDir, '_Results/intermediate/VT/', basin, '.csv'))
     samp <- floor((1-td)*nrow(train.b))
     train_ind <- sample(seq_len(nrow(train.b)), size = samp)
     
    test <- train.b[train_ind,]
    date <- test$date
    test <- test[ , !(names(test) %in% c('date','C.bname'))]
    test[] <- sapply(test, as.numeric)
    if(RFS=='shed'){
      train <- train.b[-train_ind,]
      train <- train[ , !(names(train) %in% c('date','C.bname'))]
      train[] <- sapply(train, as.numeric)
    }
    
  }else if (td == 0.5){

    samp <- floor((1-0.5)*nrow(df)) #50% train
    train_ind <- sample(seq_len(nrow(df)), size = samp)
    train <- df[-train_ind,] #
    train <- train[ , !(names(train) %in% c('date','C.bname'))]
    train[] <- sapply(train, as.numeric)
    print(nrow(train))
    test <- subset(df, df$C.bname == basin)
    samp <- floor((1-0.5)*nrow(test))
    train_ind <- sample(seq_len(nrow(test)), size = samp)
    test <- test[train_ind,]
    date <- test$date
    test <- test[ , !(names(test) %in% c('date','C.bname'))]
    test[] <- sapply(test, as.numeric)
    print(nrow(test))
    
  }else
    {
    
    if (RFS=='shed'){
      train <- subset(df, df$C.bname == basin)
      train$date <- as.Date(train$date, format= "%Y-%m-%d")
      test <- train
      if(year == 56){test <- subset(test, test$date >= "2005-01-01" & test$date <= "2006-12-31")}
      if(year == 23){test <- subset(test, test$date >= "2002-01-01" & test$date <= "2003-12-31")}
      if(year == 34){test <- subset(test, test$date >= "2003-01-01" & test$date <= "2004-12-31")}
      if(year == 59){test <- subset(test, test$date >= "2005-01-01" & test$date <= "2005-12-31" | 
                                      test$date >= "2009-01-01" & test$date <= "2009-12-31")}
      if(year == 23){test <- subset(test, test$date >= "2002-01-01" & test$date <= "2003-12-31")}
      if(year == 78){test <- subset(test, test$date >= "2007-01-01" & test$date <= "2008-12-31")}
      if(year == 67){test <- subset(test, test$date >= "2006-01-01" & test$date <= "2007-12-31")}
      if(year == 45){test <- subset(test, test$date >= "2004-01-01" & test$date <= "2005-12-31")}
      if(year == 12){test <- subset(test, test$date >= "2001-01-01" & test$date <= "2002-12-31")}
      if(year == 1){test <- subset(test, test$date >= "2000-01-01" & test$date <= "2001-12-31")}
      
      train <- setdiff(train,test)
      train <- train[ , !(names(train) %in% c('date','C.bname'))]
      train[] <- sapply(train, as.numeric)
      
      date <- test$date
      test <- test[ , !(names(test) %in% c('date','C.bname'))]
      test[] <- sapply(test, as.numeric)
      
    }else{
      train <- subset(df, df$C.bname != basin ) 
      test0 <- subset(df, df$C.bname == basin)
      test <- test0
      test0$date <- as.Date(test0$date, format= "%Y-%m-%d")
      test$date <- as.Date(test$date, format= "%Y-%m-%d")
      if(year == 56){test <- subset(test, test$date >= "2005-01-01" & test$date <= "2006-12-31")}
      if(year == 23){test <- subset(test, test$date >= "2002-01-01" & test$date <= "2003-12-31")}
      if(year == 59){test <- subset(test, test$date >= "2005-01-01" & test$date <= "2005-12-31" | 
                                      test$date >= "2009-01-01" & test$date <= "2009-12-31")}
      if(year == 23){test <- subset(test, test$date >= "2002-01-01" & test$date <= "2003-12-31")}
      if(year == 78){test <- subset(test, test$date >= "2007-01-01" & test$date <= "2008-12-31")}
      if(year == 45){test <- subset(test, test$date >= "2004-01-01" & test$date <= "2005-12-31")}
      if(year == 12){test <- subset(test, test$date >= "2001-01-01" & test$date <= "2002-12-31")}
      if(year == 1){test <- subset(test, test$date >= "2000-01-01" & test$date <= "2001-12-31")}
      
      train1 <- setdiff(test0,test)
      train <- rbind(train,train1)
      train <- train[ , !(names(train) %in% c('date','C.bname'))]
      train[] <- sapply(train, as.numeric)
      
      date <- test$date
      test <- test[ , !(names(test) %in% c('date','C.bname'))]
      test[] <- sapply(test, as.numeric)
      
    #  train.control <- trainControl(method = "LOOCV")
     # model <- train(O.obs ~., data = test, method = "ranger",
      #               trControl = train.control)
    #  print(model)
      
      
    }
    
  }

  #model training and testing including bias correction
  RF <- ranger(O.obs ~ ., data=train, importance='permutation', keep.inbag=T) #should be re-run 
  val.rf <- predict(RF, test, type='se')
  train$O.obs <-  RF$predictions - train$O.obs #bias
  RF1 <-  ranger(O.obs ~ ., data=train, importance='permutation', keep.inbag = T)
  val.rf.bc <- predict(RF1, test, type='se')
  val.rf$predictions.bc <- val.rf$predictions - val.rf.bc$predictions
  val.rf$predictions.bc <- ifelse(val.rf$predictions.bc < 0 ,val.rf$predictions, 
                                  val.rf$predictions.bc)
  val.rf$se.bc <- val.rf$se - val.rf.bc$se 
  val.rf$se.bc <- ifelse(val.rf$se.bc < 0 ,val.rf$se, val.rf$se.bc)
  
  val.join <- data.frame(cbind(test$O.obs,val.rf$predictions.bc, 
                                  val.rf$predictions,val.rf$se.bc, val.rf$se))
  
  val.join <- cbind(val.join, as.Date(date))
  names (val.join) <- c('observed','predicted_bc','predicted','se_bc', 'se', 'date')
  summary(lm(predicted_bc~observed,val.join))
  
  setwd(predDir)
  
  save(RF,file =  paste0(basin,'_',RFS,'_',td,".RData"))
  write.csv(val.join, paste0(basin,'_',RFS,'_',td,'_pred.csv'), row.names=F)
  setwd(mainDir)
  return(val.join)
  rm(RF,RF1,train,test)
  gc()
}
