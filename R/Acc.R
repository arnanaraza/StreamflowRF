### FUNCTION TO EXTRACT R2, NSE, AND PBIAS

Acc <- function(df,str){
  R <- R2(df$predicted, df$observed)
  NSE <- NSE(df$predicted, df$observed)
  PBIAS <- pbias(df$predicted, df$observed)
  RMSE <- sqrt( mean( (df$predicted-df$observed)^2) ) / ( max(df$observed)-min(df$observed) )

  R1 <- R2(df$predicted_bc, df$observed)
  NSE1 <- NSE(df$predicted_bc, df$observed)
  PBIAS1 <- pbias(df$predicted_bc, df$observed)
  RMSE1 <- sqrt( mean( (df$predicted_bc-df$observed)^2) ) / ( max(df$observed)-min(df$observed) )
  
  df1 <- data.frame(str, R,NSE,PBIAS,RMSE,R1,NSE1,PBIAS1,RMSE1 )
  return(df1)
  
}

