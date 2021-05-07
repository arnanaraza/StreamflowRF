
df.all=all.df1
df.pca=pca.df1
df.shed=shed.df1
df.basin=basin.df1


BoxPlt1 <- function(df1,df2,df3,df4,shed_name,best){

  
    df1 <- as.data.frame(df1 %>% group_by(month=month(df1$date)) %>% 
                           summarise(observed=mean(observed), predicted=mean(predicted), 
                                     se=mean(se), bias=mean(observed-predicted)/se, 
                         bias_bc=mean(observed-predicted_bc)/se))
    df2 <- as.data.frame(df2 %>% group_by(month=month(df2$date)) %>% 
                           summarise(observed=mean(observed), predicted=mean(predicted), 
                                     se=mean(se), bias=mean(observed-predicted)/se,
                         bias_bc=mean(observed-predicted_bc)/se))

    df3 <- as.data.frame(df3 %>% group_by(month=month(df3$date)) %>% 
                           summarise(observed=mean(observed), predicted=mean(predicted), 
                                     se=mean(se), bias=mean(observed-predicted)/se,
                                     bias_bc=mean(observed-predicted_bc)/se))
    
    df4 <- as.data.frame(df4 %>% group_by(month=month(df4$date)) %>% 
                           summarise(observed=mean(observed), predicted=mean(predicted), 
                                     se=mean(se), bias=mean(observed-predicted)/se,
                                     bias_bc=mean(observed-predicted_bc)/se))
    
   
  

  df1$method <- 'PCA-clustered'
  df2$method <- 'Basin-clustered'
  df3$method <- 'One-clustered'
  df4$method <- 'Watershed-level'

#  df <- rbind(df1,df2, df3, df4)  


  if(best == 1){
    df <- df1
    df <- rbind(df,df)
    df$bias[13:24]<-df$bias_bc[13:24]
    df$method[13:24]<-'Bias-adjusted'
    }
  else if(best == 2){
    df <- df2
    df <- rbind(df,df)
    df$bias[13:24]<-df$bias_bc[13:24]
    df$method[13:24]<-'Bias-adjusted'
    }
  else if(best == 3){
    df <- df3
    df <- rbind(df,df)
    df$bias[13:24]<-df$bias_bc[13:24]
    df$method[13:24]<-'Bias-adjusted'
    }
  else{
    df==df4
    df <- rbind(df,df)
    df$bias[13:24]<-df$bias_bc[13:24]
    df$method[13:24]<-'Bias-adjusted'
    }
  names(df) <- c('month', 'observed', 'predicted', 'Uncertainty','SR','SR_bc', 'method')
  cols <- c("red", "blue")
  
  p <- ggplot(df, aes(x=month, y=SR)) + 
    scale_colour_manual(name="method", values=cols,limits=c(unique(df$method)[1],'Bias-adjusted')) +
    geom_point(aes(colour = method), size=2.5)+
    theme_bw()+theme(legend.position = "none")+ggtitle(shed_name)+ 
    scale_x_continuous("Month", breaks=c(1:12)) +
    scale_y_continuous("Standardized residuals",limits=c(-2.2,2.2)) +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.75) + 
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.x=element_blank(), 
          text = element_text(size=15))
  
  g=ggplot(df, aes(x=month, y=SR)) + 
    scale_colour_manual(name="method", values=cols,limits=c('Best method','Bias-adjusted')) +
    geom_point(aes(colour = method), size=2.5)+
    theme_bw()+theme(legend.position = "bottom")+ggtitle(shed_name)+ 
    scale_x_continuous("Month", breaks=c(1:12)) +
    scale_y_continuous("Standardized residuals",limits=c(-2.2,2.2)) +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.75) + 
    theme(axis.ticks.x=element_blank(), 
          text = element_text(size=15))
  return(p)
  
}

best.mod <- all.pred[,c()]

best <- c(3,2,3,3,2,2,3,1,1,3,2,2,1,3,2,1,2,3,1,2,1) #########!!!!!!!!!!!!
boxplt <- lapply(1:21, function(x) BoxPlt1(df.pca[[x]], df.basin[[x]], df.all[[x]],
                                            df.shed[[x]],SW.list[[x]], best[[x]]))
setwd(predDir)
ggsave( multiplot(boxplt[[1]], boxplt[[2]],boxplt[[3]], boxplt[[4]], boxplt[[5]],
                  boxplt[[6]], boxplt[[7]],boxplt[[8]], boxplt[[9]], boxplt[[10]],
                  boxplt[[11]], boxplt[[12]],boxplt[[13]], boxplt[[14]], boxplt[[15]],
                  boxplt[[16]], boxplt[[17]],boxplt[[18]], boxplt[[19]], boxplt[[20]],
                  boxplt[[21]],cols=3),filename='PaperFigure_Season1.png',device='png',
        dpi=600, width = 8, height = 16, units='in' )








