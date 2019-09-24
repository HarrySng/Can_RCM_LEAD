library(ncdf4)
library(doParallel)

args = commandArgs(trailingOnly=TRUE)
dd <- args[1]
vars_vector <- c('prAdjust','tasmaxAdjust','tasminAdjust')
indls <- readRDS('canlead_zone_ind.rds')

run_vector <- c('r1_r1i1p1','r1_r2i1p1','r1_r3i1p1','r1_r4i1p1','r1_r5i1p1','r1_r6i1p1','r1_r7i1p1','r1_r8i2p1','r1_r9i2p1','r1_r10i2p1','r2_r1i1p1','r2_r2i1p1','r2_r3i1p1','r2_r4i1p1','r2_r5i1p1','r2_r6i1p1','r2_r7i1p1','r2_r8i2p1','r2_r9i2p1','r2_r10i2p1','r3_r1i1p1','r3_r2i1p1','r3_r3i1p1','r3_r4i1p1','r3_r5i1p1','r3_r6i1p1','r3_r7i1p1','r3_r8i2p1','r3_r9i2p1','r3_r10i2p1','r4_r1i1p1','r4_r2i1p1','r4_r3i1p1','r4_r4i1p1','r4_r5i1p1','r4_r6i1p1','r4_r7i1p1','r4_r8i2p1','r4_r9i2p1','r4_r10i2p1','r5_r1i1p1','r5_r2i1p1','r5_r3i1p1','r5_r4i1p1','r5_r5i1p1','r5_r6i1p1','r5_r7i1p1','r5_r8i2p1','r5_r9i2p1','r5_r10i2p1')

data_maker <- function(fname, var, inds) {
  ncf <- nc_open(fname)
  data <- ncvar_get(ncf, var)
  data <- matrix(data, dim(data)[1]*dim(data)[2], dim(data)[3]) # Current dim = grid * time
  data <- t(data) # New dim = Time * grid
  zone_list <- list()
  for (i in 1:15) {
    m <- data[,inds[[i]]$indx]
    zone_list[[i]] <- rowMeans(m)
    rm(m) ; gc()
  }
  return(zone_list)
}

for (i in 1:50) {
  selected_run <- run_vector[i]
  fls <- list.files(path = paste('./',dd,'/',sep=""), pattern = paste(selected_run), full.names = T)
  print(i) ; print(fls)
  
  ncores <- 3
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  print('Starting foreach')
  resls <- foreach(fn = fls, var = vars_vector, 
                   .packages=c('ncdf4'), .export = c()) %dopar% {
                     data_maker(fn, var, indls)
                   }
  stopCluster(cl)
  
  if(dd == 'rawnc') {
    saveRDS(resls,paste('./onerun_cld2/Run',i,'.rds',sep=""),compress = F)
  } else if (dd == 'rawnc1') {
    saveRDS(resls,paste('./onerun_cld1/Run',i,'.rds',sep=""),compress = F)
  }
  
  rm(resls) ; gc()
  print('End of one run')
}

q()
