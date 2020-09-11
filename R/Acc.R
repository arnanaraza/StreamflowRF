### FUNCTION TO EXTRACT R2, NSE, AND PBIAS

Acc <- function(df,str){
  R <- R2(df$predicted, df$observed)
  NSE <- NSE(df$predicted, df$observed)
  PBIAS <- pbias(df$predicted, df$observed)
  df1 <- data.frame(str, R,NSE,PBIAS)
  return(df1)
  
}
