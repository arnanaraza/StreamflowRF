
# Function to predict using RF model then 
predTest <- function (td=0.6, df=mrb.VT, basin='mrb_s', RFS='all',predDir,year) {
  dir.create(predDir, showWarnings = FALSE)
  
  #prepare training-test data 
  set.seed(123)
  train <- subset(df, df$C.bname != basin)
  train$date <- as.Date(train$date, format= "%Y-%m-%d")
  test0 <- subset(df, df$C.bname == basin)
  test0$date <- as.Date(test0$date, format= "%Y-%m-%d")
  test <- test0
  
  #create folds
  test$year <- format(test$date,"%Y") 
  na_obs <- test %>% 
    group_by(year) %>% 
    summarise_at(vars(starts_with("O.obs")), ~sum(is.na(.)))
  folds <- lapply(na_obs$year, function(x) subset(test, test$year == x))
  
  #remove after the folds
  folds <- lapply(folds, function(x) { x["year"] <- NULL; x })
  test <- test[,-length(test)]
  
  #x-val
  val.list <- list()
  for (i in 1: length(folds)){
    train <- rbind(train, setdiff(test,folds[[i]]))
    test <- folds[[i]]
    
  #  train <- train[ , !(names(train) %in% c('date','C.bname'))]
 #   train[] <- sapply(train, as.numeric)
    date <- test$date
 #   test <- test[ , !(names(test) %in% c('date','C.bname'))]
    
    #model training and testing including bias correction
    RF <- ranger(train$O.obs ~ ., data=train[,-17], importance='permutation',mtry=15, keep.inbag=T) #should be re-run 
    print(hist(RF$predictions))
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
    print(summary(lm(predicted_bc~observed,val.join)))
    print(head(val.join))
    
    gc()
    val.list[[i]] <- val.join
    }
  
  
  return(val.list)
#  setwd(predDir)
 # save(RF,file =  paste0(basin,'_',RFS,'_',td,".RData"))
  #write.csv(val.join, paste0(basin,'_',RFS,'_',td,'_pred.csv'), row.names=F)
  #setwd(mainDir)
}
  
asdf=ldply(val.list,data.frame)
 
  
