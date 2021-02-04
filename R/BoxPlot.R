
df.all=all.df1
df.pca=pca.df1
df.shed=shed.df1
df.basin=basin.df1

BoxPlt1 <- function(df1,df2,df3,df4,shed_name,bias){
  if (bias =='default'){
    df1 <- as.data.frame(df1 %>% group_by(month=month(df1$date)) %>% 
                           summarise(observed=mean(observed), predicted=mean(predicted), 
                                     se=mean(se), bias=mean(observed-predicted)/se))
    df2 <- as.data.frame(df2 %>% group_by(month=month(df2$date)) %>% 
                           summarise(observed=mean(observed), predicted=mean(predicted), 
                                     se=mean(se), bias=mean(observed-predicted)/se))
    df3 <- as.data.frame(df3 %>% group_by(month=month(df3$date)) %>% 
                           summarise(observed=mean(observed), predicted=mean(predicted), 
                                     se=mean(se), bias=mean(observed-predicted)/se))
    df4 <- as.data.frame(df4 %>% group_by(month=month(df4$date)) %>% 
                           summarise(observed=mean(observed), predicted=mean(predicted), 
                                     se=mean(se), bias=mean(observed-predicted)/se))
  }else{
    df1 <- as.data.frame(df1 %>% group_by(month=month(df1$date)) %>% 
                           summarise(observed=mean(observed), predicted=mean(predicted_bc), 
                                     se_bc=mean(se), bias=mean(observed-predicted_bc)/se_bc))
    df2 <- as.data.frame(df2 %>% group_by(month=month(df2$date)) %>% 
                           summarise(observed=mean(observed), predicted=mean(predicted_bc), 
                                     se_bc=mean(se), bias=mean(observed-predicted_bc)/se_bc))
    df3 <- as.data.frame(df3 %>% group_by(month=month(df3$date)) %>% 
                           summarise(observed=mean(observed), predicted=mean(predicted_bc), 
                                     se_bc=mean(se), bias=mean(observed-predicted_bc)/se_bc))
    df4 <- as.data.frame(df4 %>% group_by(month=month(df4$date)) %>% 
                           summarise(observed=mean(observed), predicted=mean(predicted_bc), 
                                     se_bc=mean(se), bias=mean(observed-predicted_bc)/se_bc))
  }

  df1$method <- 'PCA-clustered'
  df2$method <- 'Basin-clustered'
  df3$method <- 'One-clustered'
  df4$method <- 'Watershed-level'
  
  df <- rbind(df1,df2, df3, df4)  
  
  names(df) <- c('month', 'observed', 'predicted', 'Uncertainty','SR', 'method')
  cols <- c("blue","steelblue", "lightblue", "black")
  
  
  p <- ggplot(df, aes(x=month, y=SR)) + 
    scale_colour_manual(name="RF models", values=cols,limits=c('PCA-clustered', 'Basin-clustered',
                                                               'One-clustered', 'Watershed-level')) +
    geom_point(aes(colour = method), size=2.5)+
    theme_bw()+theme(legend.position = "none")+ggtitle(shed_name)+ 
    scale_x_continuous("Month", breaks=c(1:12)) +
    scale_y_continuous("Standardized residuals",limits=c(-2.2,2.2)) +
    geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.75) + 
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.x=element_blank(), 
          text = element_text(size=15))
  return(p)
}

boxplt <- lapply(1:21, function(x) BoxPlt1(df.pca[[x]], df.basin[[x]], df.all[[x]],
                                            df.shed[[x]],SW.list[[x]], 'default'))
boxplt1 <- lapply(1:21, function(x) BoxPlt1(df.pca[[x]], df.basin[[x]], df.all[[x]],
                                            df.shed[[x]],SW.list[[x]], 'bc'))

ggsave( multiplot(boxplt[[1]], boxplt[[2]],boxplt[[3]], boxplt[[4]], boxplt[[5]],
                  boxplt[[6]], boxplt[[7]],boxplt[[8]], boxplt[[9]], boxplt[[10]],
                  boxplt[[11]], boxplt[[12]],boxplt[[13]], boxplt[[14]], boxplt[[15]],
                  boxplt[[16]], boxplt[[17]],boxplt[[18]], boxplt[[19]], boxplt[[20]],
                  boxplt[[21]],cols=3),filename='PaperFigure_BiasShed.png',device='png',
        dpi=600, width = 8, height = 16, units='in' )
ggsave( multiplot(boxplt1[[1]], boxplt1[[2]],boxplt1[[3]], boxplt1[[4]], boxplt1[[5]],
                  boxplt1[[6]], boxplt1[[7]],boxplt1[[8]], boxplt1[[9]], boxplt1[[10]],
                  boxplt1[[11]], boxplt1[[12]],boxplt1[[13]], boxplt1[[14]], boxplt1[[15]],
                  boxplt1[[16]], boxplt1[[17]],boxplt1[[18]], boxplt1[[19]], boxplt1[[20]],
                  boxplt1[[21]],cols=3),filename='PaperFigure_BiasShedBC.png',device='png',
        dpi=600, width = 8, height = 16, units='in' )
  








