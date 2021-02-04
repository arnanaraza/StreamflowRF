

setwd(predDir)
load("aarb_a_all_0.4.Rdata")
vim = as.data.frame(importance(RF))
vim$covs <- row.names(vim)
vim$temporal <- c()


p1 <- vip(RF)  # model-specific
