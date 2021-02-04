
# Function to predict using RF model then 
predTest <- function (td=0.6, df=mrb.VT, basin='mrb_s', RFS='all',predDir) {
  dir.create(predDir, showWarnings = FALSE)
  
  #prepare training-test data 
  set.seed(123)
  training <- subset(df, df$C.bname != basin)
  training$date <- as.Date(training$date, format= "%Y-%m-%d")
  testing <- subset(df, df$C.bname == basin)
  testing$date <- as.Date(testing$date, format= "%Y-%m-%d")
  
  #create folds
  testing$year <- format(testing$date,"%Y") 
  na_obs <- testing %>% 
    group_by(year) %>% 
    summarise_at(vars(starts_with("O.obs")), ~sum(is.na(.)))
  folds <- lapply(na_obs$year, function(x) subset(testing, testing$year == x))
  
  #remove year after the folds
  folds <- lapply(folds, function(x) { x["year"] <- NULL; x })
  test1 <- testing[,-length(testing)]
  
  #x-val
  val.list <- list()
  
  for (i in 1:length(folds)){ #for every year!
    if(RFS=='shed'){
      train <-  setdiff(test1,folds[[i]])#td for shed, extra td for others
      test <- setdiff(test1,train)
    #  train <-  rbind(train, sample_n(folds[[i]], nrow(folds[[i]])*0.05))

    }else{
      train <- rbind(training,setdiff(test1,folds[[i]]))#, sample_n(test1, nrow(test1)*0.05))
      test <- folds[[i]]
    }
    
    train <- train[ , !(names(train) %in% c('date','C.bname'))]
    train[] <- sapply(train, as.numeric)
    date <- test$date
    test <- test[ , !(names(test) %in% c('date','C.bname'))]
    test[] <- sapply(test, as.numeric)
    
    #model training and testing including bias correction
    RF <- ranger(train$O.obs ~ ., data=train[,-17], importance='permutation', mtry=15, keep.inbag=T) #should be re-run 
    val.rf <- predict(RF, test[-17], type='se')
    train$O.obs <-  RF$predictions - train$O.obs #bias
    RF1 <-  ranger(train$O.obs ~ ., data=train[,-17], importance='permutation',mtry=15, keep.inbag = T)
    val.rf.bc <- predict(RF1, test[-17], type='se')
    val.rf$predictions.bc <- val.rf$predictions - val.rf.bc$predictions
    val.rf$predictions.bc <- ifelse(val.rf$predictions.bc < 0 ,val.rf$predictions, 
                                    val.rf$predictions.bc)
    val.rf$se.bc <- val.rf$se - val.rf.bc$se 
    val.rf$se.bc <- ifelse(val.rf$se.bc < 0 ,val.rf$se, val.rf$se.bc)
    
    val.join <- data.frame(cbind(test$O.obs,val.rf$predictions.bc, 
                                 val.rf$predictions,val.rf$se.bc, val.rf$se))
    
    val.join <- cbind(val.join, as.Date(date))
    names (val.join) <- c('observed','predicted_bc','predicted','se_bc', 'se', 'date')
    print(summary(lm(observed~predicted_bc, val.join)))
    val.list[[i]] <- val.join
    gc()
    rm(RF1,train,test)
  }
  setwd(predDir)
  save(RF,file =  paste0(basin,'_',RFS,'_',td,".RData"))
  write.csv(ldply(val.list,data.frame), paste0(basin,'_',RFS,'_',td,'_pred.csv'), row.names=F)
  setwd(mainDir)
}





