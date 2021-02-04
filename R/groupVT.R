### FUNCTION TO CREATE PRE-DEFINED WATERSHED GROUPING METHODS FOR VALUE TABLE ASSEMBLY 

groupVT <- function(basin){
  
  # PCA-based on 'clstr' variable
  if (basin == 'pca1'){
    SW.list <- SW.list[clstr==1]
    print(paste(SW.list, 'is PCA1'))
    SW.conv <-rep('no', length(SW.list))
  }
  if (basin == 'pca2'){
    SW.list <- SW.list[clstr==2]  
    print(paste(SW.list, 'is PCA2'))
    SW.conv <-rep('no', length(SW.list))
  }
  if (basin == 'pca3'){
    SW.list <- SW.list[clstr==3]   

    print(paste(SW.list, 'is PCA3'))
    SW.conv <-rep('no', length(SW.list))
  }
  if (basin == 'pca4'){
    SW.list <- SW.list[clstr==4]  
    print(paste(SW.list, 'is PCA4'))
    SW.conv <-rep('no', length(SW.list))
  }
  if (basin == 'pca5'){
    SW.list <- SW.list[clstr==5]  
    print(paste(SW.list, 'is PCA5'))
    SW.conv <-rep('no', length(SW.list))
  }
  if (basin == 'pca6'){
    SW.list <- SW.list[clstr==6]  
    print(paste(SW.list, 'is PCA6'))
    SW.conv <-rep('no', length(SW.list))
  }
  if (basin == 'pca7'){
    SW.list <- SW.list[clstr==7]  
    print(paste(SW.list, 'is PCA7'))
    SW.conv <-rep('no', length(SW.list))
  }
  
  # group all watersheds
  if (basin == 'all'){
    SW.list <- c('aarb_a', 'aarb_n', 'abrb_s','arb_b', 'arb_c', 'crb_a', 'crb_be', 
                 'crb_bu', 'crb_d', 'crb_j','crb_m','crb_p', 'crb_s', 
                 'crb_t', 'crb_u','mrb_s', 'prb_b', 'prb_c', 'prb_a', 
                 'prb_p', 'prb_r')    
    SW.conv <- c ('yes', 'no', 'no', 'no', 'no','yes', 'no', 'no', 'no','no',
                  'no','no','no','no','no','no','no','yes', 'no', 'no')
    }
  
  #group per mother basin
  if (basin == 'aarb'){
    SW.list <-  c('aarb_a', 'aarb_n')
    SW.conv <- c ('yes', 'no')}
  if (basin == 'abrb'){
    SW.list <-  'abrb_s'
    SW.conv <-'no'}
  if (basin == 'arb'){
    SW.list <-  c('arb_b', 'arb_c')
    SW.conv <- c ('no', 'no')}
  if (basin == 'crb'){
    SW.list <- c('crb_a', 'crb_be', 'crb_bu', 'crb_d', 'crb_j','crb_m','crb_p', 'crb_s', 'crb_t', 'crb_u')
    SW.conv <- c('yes', 'no', 'no', 'no','no','no','no','no','no','no')}
  if (basin == 'mrb'){
    SW.list <-  'mrb_s'
    SW.conv <-'no'}
  if (basin == 'prb'){
    SW.list <- c('prb_b', 'prb_c', 'prb_a', 'prb_p', 'prb_r')
    SW.conv <- c('no','no','yes', 'no', 'no')}

  # no grouping, stand-alone watershed tables
  if (basin == 'aarb_a'){
    SW.list <-  c('aarb_a')
    SW.conv <- c ('no')}
  if (basin == 'aarb_n'){
    SW.list <-  c('aarb_n')
    SW.conv <- c ('no')}
  if (basin == 'abrb_s'){
    SW.list <-  c('abrb_s')
    SW.conv <- c ('no')}
  if (basin == 'arb_b'){
    SW.list <-  c('arb_b')
    SW.conv <- c ('no')}
  if (basin == 'arb_c'){
    SW.list <-  c('arb_c')
    SW.conv <- c ('no')}
  if (basin == 'crb_a'){
    SW.list <-  c('crb_a')
    SW.conv <- c ('no')}
  if (basin == 'crb_be'){
    SW.list <-  c('crb_be')
    SW.conv <- c ('no')}
  if (basin == 'crb_bu'){
    SW.list <-  c('crb_bu')
    SW.conv <- c ('no')}
  if (basin == 'crb_d'){
    SW.list <-  c('crb_d')
    SW.conv <- c ('no')}
  if (basin == 'crb_j'){
    SW.list <-  c('crb_j')
    SW.conv <- c ('no')}
  if (basin == 'crb_m'){
    SW.list <-  c('crb_m')
    SW.conv <- c ('no')}
  if (basin == 'crb_p'){
    SW.list <-  c('crb_p')
    SW.conv <- c ('no')}
  if (basin == 'crb_s'){
    SW.list <-  c('crb_s')
    SW.conv <- c ('no')}
  if (basin == 'crb_t'){
    SW.list <-  c('crb_t')
    SW.conv <- c ('no')}
  if (basin == 'crb_u'){
    SW.list <-  c('crb_u')
    SW.conv <- c ('no')}
  if (basin == 'mrb_s'){
    SW.list <-  c('mrb_s')
    SW.conv <- c ('no')}
  if (basin == 'prb_a'){
    SW.list <-  c('prb_a')
    SW.conv <- c ('no')}
  if (basin == 'prb_b'){
    SW.list <-  c('prb_b')
    SW.conv <- c ('no')}
  if (basin == 'prb_c'){
    SW.list <-  c('prb_c')
    SW.conv <- c ('no')}
  if (basin == 'prb_p'){
    SW.list <-  c('prb_p')
    SW.conv <- c ('no')}
  if (basin == 'prb_r'){
    SW.list <-  c('prb_r')
    SW.conv <- c ('no')}
  
  
  # Wrapper for the data frame assembly of value tables
  source('R/shedVT.R')
  all.VT <- lapply(SW.list, function(x) shedVT(x, SW.conv)) # yes are aarb_a, crb_a, prb_a
  all.VT1 <- lapply(all.VT, function(x) x[-c(1:730),]) #remove <year 2000
  
  # value table formatting
  vt.names <- c('LC1.agr', 'LC2.for', 'LC3.gr', 'LC4.wgr', 'LC5.wetl', 
                'LC6.riv', 'LC7.bu', 'LC8.barr', 'LC9.CN', 'LC10.man',
                'LC11.fl', 'W1.ws', 'W2.tmin', 'W3.tmax', 'W4.pcp', 'W5.rh', 'O.obs',
                'P.bd1', 'P.br1', 'P.cl1', 'P.sa1', 'P.si1', 'P.dem1', 'P.sl1', 
                'P.bd2', 'P.br2', 'P.cl2', 'P.sa2', 'P.si2', 'P.dem2', 'P.sl2',
                'P.bd3', 'P.br3', 'P.cl3', 'P.sa3', 'P.si3', 'P.dem3', 'P.sl3',
                'P.bd4', 'P.br4', 'P.cl4', 'P.sa4', 'P.si4', 'P.dem4', 'P.sl4', 
                'P.bd5', 'P.br5', 'P.cl5', 'P.sa5', 'P.si5', 'P.dem5', 'P.sl5', 
                'date', 'W6.pcpw', 'W7.pcpm', 'W8.pcpmx', 'C.area', 'C.basin', 'C.size', 'C.clim', 
                'C.bname')
  all.VT2 <- lapply(all.VT1, setNames, nm = vt.names)
  all.VT3 <- lapply(all.VT2, function(x) unfactor(x))

  all.VT.fin <- do.call("rbind", all.VT3)
  all.VT.fin <- na.omit(all.VT.fin)
  all.VT.fin$mo <- month(all.VT.fin$date)
  vt.names <- c(vt.names, 'C.mo')
  colnames(all.VT.fin) <- vt.names
  all.VT.fin$date <- as.Date(all.VT.fin$date)
  all.VT.fin$O.obs <- all.VT.fin$O.obs/1000 #into m3/sec
  
  setwd(vtDir)
  write.csv(all.VT.fin, paste0(basin,'.csv'), row.names = F)
  setwd(mainDir)
  
  
  # create training-testing data
#  if (td < 1){
 #   set.seed(123) 
  #  train <- sample(seq_len(nrow(all.VT.fin)), size = floor((1-td)*nrow(all.VT.fin)))
   # train <- all.VT.fin[-train,]}
  #if (td == 1){
   # train <- all.VT.fin}

  
#  train[] <- sapply(train, as.numeric)
return (all.VT.fin)  
  
}





