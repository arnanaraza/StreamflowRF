
# Function to predict using RF model then 
predTest <- function (td=0.6, df=mrb.VT.yes, basin='mrb_s', RFS='all',predDir) {
  df0 <- df
  
  #prepare training-test data 
  set.seed(123)
  if (td < 1){
    
    #prepare train-test data
    train <- subset(df, df$C.bname != basin) 
    train.b <- subset(df, df$C.bname == basin) 
    samp <- floor((1-td)*nrow(train.b))
    train_ind <- sample(seq_len(nrow(train.b)), size = samp)
    train.b1 <- train.b[-train_ind,] #
    train <- rbind(train,train.b1)
    train <- train[ , !(names(train) %in% c('date','C.bname'))]
    train[] <- sapply(train, as.numeric)
    
    #test <- setdiff( subset(df, df$C.bname == basin), train.b)
    test <- train.b[train_ind,]
    date <- test$date
    test <- test[ , !(names(test) %in% c('date','C.bname'))]
    test[] <- sapply(test, as.numeric)
    
  }else if (td == 0.4){

    #prepare train-test data
    samp <- floor((1-0.4)*nrow(df)) #40% train
    train_ind <- sample(seq_len(nrow(df)), size = samp)
    train <- df[-train_ind,] #
    train <- train[ , !(names(train) %in% c('date','C.bname'))]
    train[] <- sapply(train, as.numeric)
    
    #test <- setdiff( subset(df, df$C.bname == basin), train.b)
    test <- subset(df, df$C.bname == basin)
    samp <- floor((1-0.4)*nrow(test))
    train_ind <- sample(seq_len(nrow(test)), size = samp)
    test <- test[train_ind,]
    date <- test$date
    test <- test[ , !(names(test) %in% c('date','C.bname'))]
    test[] <- sapply(test, as.numeric)
    
  }else{
    df <- df[ , !(names(df) %in% 'date')]
    
    #prepare train-test data
    train <- subset(df, df$C.bname != basin) 
    train <- train[ , !(names(train) %in% 'C.bname')]
    train[] <- sapply(train, as.numeric)
    
    test <- subset(df, df$C.bname == basin) 
    test <- test[ , !(names(test) %in% 'C.bname')]
    test[] <- sapply(test, as.numeric)
    
  }
  #model training and testing including bias correction
  RF <- ranger(O.obs ~ ., data=train, importance='permutation', mtry=15, keep.inbag=T) #should be re-run 
  print(importance(RF))
  val.rf <- predict(RF, test[-17], type='se')
  train$O.obs <-  RF$predictions - train$O.obs #bias
  RF1 <-  ranger(O.obs ~ ., data=train, importance='permutation',mtry=15, keep.inbag = T)
  val.rf.bc <- predict(RF1, test[-17], type='se')
  
  val.rf$predictions.bc <- val.rf$predictions - val.rf.bc$predictions
  #val.rf$predictions.bc <- 2*val.rf$predictions - val.rf.bc$predictions
  val.rf$predictions.bc <- ifelse(val.rf$predictions.bc < 0 ,0, val.rf$predictions.bc)
 # val.rf$se.bc <- val.rf$se - (val.rf.bc$se - val.rf$se)
  val.rf$se.bc <- val.rf$se - val.rf.bc$se 
  val.rf$se.bc <- ifelse(val.rf$se.bc < 0 ,val.rf$se, val.rf$se.bc)
  
  val.join <- data.frame(cbind(test$O.obs,val.rf$predictions.bc, 
                                  val.rf$predictions,val.rf$se.bc, val.rf$se))
  
  #date <- subset(df0, df0$C.bname == basin)$date ###!
  val.join <- cbind(val.join, as.Date(date))
  
  #plot regression graphs
  names (val.join) <- c('observed','predicted_bc','predicted','se_bc', 'se', 'date')
  setwd(predDir)
  write.csv(val.join, paste0(basin,'_',RFS,'_',td,'_pred.csv'), row.names=F)
  setwd(mainDir)
  return(val.join)
  gc()
}
