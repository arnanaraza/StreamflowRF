
# Function to predict using RF model then 
predTest <- function (td, basin, RF, RFS) {
  p <- td*100
  all.VT.fin <- read.csv(paste0(vtDir, basin,'_',p,'.csv'))
  names(all.VT.fin) <- c(names(all.VT.fin[-length(all.VT.fin)]), 'C.mo')
  #prepare training-test data
  set.seed(123)
  samp <- floor((1-td)*nrow(all.VT.fin))
  train_ind <- sample(seq_len(nrow(all.VT.fin)), size = samp)
  train <- all.VT.fin[-train_ind,]
  test <- all.VT.fin[ !(rownames(all.VT.fin) %in% rownames(train)), ]
  val.rf <- predict(RF, test[-17], type='se')
  val.join <- as.data.frame(cbind(test[[17]], val.rf$predictions, val.rf$se))
  date <- test$date
  val.join <- cbind(val.join, as.Date(date))
  
  #use VT name for outputs export
  df.name <- basin
  
  #plot regression graphs
  colnames (val.join) <- c('observed','predicted','se', 'date')
  setwd(predDir)
  write.csv(val.join, paste0(basin,'_',RFS,'_pred.csv'), row.names=F)
  setwd(mainDir)
  return(val.join)
}
