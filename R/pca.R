### FUNCTION TO CREATE CLUSTERS AND ASSIGN CLUSTER TO INDEPENDENT WATERSHED
### DEFAULT CLUSTERS (4)

 PCA <- function(each.VT=each.VT, clstr=3){
   set.seed(123)
   vt.list <- lapply(each.VT, function(x) x[] <- sapply(x, as.numeric))
   
   each.mean <- lapply(1:length(SW.list), function(x)
     t(colMeans(vt.list[[x]][,-c(17,53,61)]))) #don't include streamflow and dates on the clustering
   each.mean <- ldply(each.mean,data.frame)
   each.mean <- as.data.frame(scale(each.mean))
   each.mean <- each.mean[,-4] #NA-full column
   
    kmeans(each.mean, centers = clstr,nstart=50,iter.max = 1000)[[1]]

 }
 
 PCA_new <- function(val_n){
   each.mean$clstr = clstr
   clstr.rf <- ranger(as.factor(clstr) ~ ., data=each.mean)
   clstr.val <- predict(clstr.rf, each.mean.val)[[1]]
   each.mean <- each.mean[-sample(1:nrow(each.mean),val_n,replace=T),]
   each.mean.val <- each.mean[sample(1:nrow(each.mean),val_n,replace=T),]
   
 }