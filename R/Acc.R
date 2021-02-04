### FUNCTION TO EXTRACT R2, NSE, AND PBIAS

Acc <- function(df,str){
  rsq <- function (x, y) cor(x, y) ^ 2
  
  R <- rsq(df$predicted, df$observed)
  NSE <- NSE(df$predicted, df$observed)
  PBIAS <- pbias(df$predicted, df$observed)
  RMSE <- nrmse(df$predicted, df$observed, norm = "sd")
  
  R1 <- rsq(df$predicted_bc, df$observed)
  NSE1 <- NSE(df$predicted_bc, df$observed)
  PBIAS1 <- pbias(df$predicted_bc, df$observed)
  RMSE1 <- nrmse(df$predicted_bc, df$observed, norm = "sd")
  
  df1 <- data.frame(str, R,NSE,PBIAS,RMSE,R1,NSE1,PBIAS1,RMSE1 )
  return(df1)
  
}
RegLine <- function(df, title){
  rsq <- function (x, y) cor(x, y) ^ 2
  
  R <- rsq(df$predicted, df$observed)
  NSE <- NSE(df$predicted, df$observed)
  PBIAS <- pbias(df$predicted, df$observed)
  RMSE <- nrmse(df$predicted, df$observed, norm = "sd")
  R = round(R,2)
  NSE=round(NSE,2)
  PBIAS=round(PBIAS,2)
  RMSE=round(RMSE,2)
  mn=0
  mx=6000
  mn1=1000
  mx1=mx-500
  clr='gradient'

  xx=bquote('Observed Q ('*m^-3~s^-1*')')
  yy=bquote('Predicted Q ('*m^-3~s^-1*')')
  y='predicted'
  p=ggplot(df, aes_string(x='observed', y=y))+ geom_point()+
    labs(x = xx,y=yy) + 
    xlim(mn, mx) + ylim(mn, mx)+
  #  geom_hex(bins = bn) +scale_fill_continuous(type = clr) +  
    theme_bw()+
    geom_smooth(method=lm, linetype="dashed",se=FALSE,fullrange=TRUE) + 
    ggtitle(title) +
    geom_abline(slope=1, intercept=0, linetype='dashed', colour='red')+
    annotate(geom="text", x=mn1, y=mx1, size = 5,
             label=paste0('R.RMSE=',RMSE,'%'),color="black")+
    annotate(geom="text", x=mn1, y=mx1-500, size = 5,
             label=paste0('PBIAS=',PBIAS,'%'),color="black")+
    annotate(geom="text", x=mn1, y=mx1-1000, size = 5,
             label=paste0('NSE=',NSE),color="black")+
    annotate(geom="text", x=mn1, y=mx1-1500, size = 5,
             label=paste0('R sq.=',R),color="black")+
    theme(axis.line = element_line(colour = "black"),
          text = element_text(size=16),
          plot.title = element_text(margin = margin(b = -10), hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  
  y='predicted_bc'
  
  R <- rsq(df$predicted_bc, df$observed)
  NSE <- NSE(df$predicted_bc, df$observed)
  PBIAS <- pbias(df$predicted_bc, df$observed)
  RMSE <- nrmse(df$predicted_bc, df$observed, norm = "sd")
  R = round(R,2)
  NSE=round(NSE,2)
  PBIAS=round(PBIAS,2)
  RMSE=round(RMSE,2)
  
  p1=ggplot(df, aes_string(x='observed', y=y))+ geom_point()+
    labs(x = xx,y=yy) + 
    xlim(mn, mx) + ylim(mn, mx)+
    theme_bw()+
    geom_smooth(method=lm, linetype="dashed",se=FALSE,fullrange=TRUE) + 
    ggtitle(title) +
    geom_abline(slope=1, intercept=0, linetype='dashed', colour='red')+
    annotate(geom="text", x=mn1, y=mx1, size = 5,
             label=paste0('R.RMSE=',RMSE,'%'),color="black")+
    annotate(geom="text", x=mn1, y=mx1-500, size = 5,
             label=paste0('PBIAS=',PBIAS,'%'),color="black")+
    annotate(geom="text", x=mn1, y=mx1-1000, size = 5,
             label=paste0('NSE=',NSE),color="black")+
    annotate(geom="text", x=mn1, y=mx1-1500, size = 5,
             label=paste0('R sq.=',R),color="black")+
    theme(axis.line = element_line(colour = "black"),
          text = element_text(size=16),
          plot.title = element_text(margin = margin(b = -10), hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  list(p,p1)
}
