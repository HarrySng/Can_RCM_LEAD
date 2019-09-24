library(lubridate)
library(tidyverse)
library(climdex.pcic)
library(doParallel)

dts <- readRDS('RCM_Dates.rds')
dts <- as.PCICt(as.character(dts[13141:35040]), cal = '365')
source('clim_func_names.R')
func_names <- func_names[c(3,4,5,6,12,13,7,16,19,21,22,24,25,26)]
args = commandArgs(trailingOnly=TRUE)
prd <- as.numeric(args[1])

pr_base <- readRDS('./NEW/prds/pr0.rds')
tx_base <- readRDS('./NEW/prds/tasmax0.rds')
tn_base <- readRDS('./NEW/prds/tasmin0.rds')

pr_futr <- readRDS(paste('./NEW/prds/pr',prd,'.rds',sep=""))
tx_futr <- readRDS(paste('./NEW/prds/tasmax',prd,'.rds',sep=""))
tn_futr <- readRDS(paste('./NEW/prds/tasmin',prd,'.rds',sep=""))

print("Data read")

for (z in 1:15) {
  t1 <- proc.time()[3]
  print(paste('Zone >>>>>>>>>>>>',z))
  pr_base_z <- pr_base[[z]]
  tx_base_z <- tx_base[[z]]
  tn_base_z <- tn_base[[z]]
  
  pr_futr_z <- pr_futr[[z]]
  tx_futr_z <- tx_futr[[z]]
  tn_futr_z <- tn_futr[[z]]
  
  prls_combined <- list()
  txls_combined <- list()
  tnls_combined <- list()
  
  for (g in 1:length(pr_base_z)) {
    prls_combined[[g]] <- rbind(pr_base_z[[g]],pr_futr_z[[g]])
    txls_combined[[g]] <- rbind(tx_base_z[[g]],tx_futr_z[[g]])
    tnls_combined[[g]] <- rbind(tn_base_z[[g]],tn_futr_z[[g]])
  }
  
  rm(pr_base_z,tx_base_z,tn_base_z,pr_futr_z,tx_futr_z,tn_futr_z)
  gc()
  
  print('Periods combined')
  
  climmer <- function(pr,tmx,tmn) {
    pr <- pr*86400 ; tmx  <- tmx-273.15 ; tmn <- tmn-273.15
    cls <- list()
    for (p in 1:3) {
      ci <- climdexInput.raw(tmax = tmx[,p], tmin = tmn[,p], prec = pr[,p], 
                             tmax.dates = dts, tmin.dates = dts, prec.dates = dts, 
                             base.range = c(1986,2015))
      climls <- list()
      for (i in 1:length(func_names)) {
        climls[[i]] <- eval(parse(text = func_names[i]))(ci)
      }
      cls[[p]] <- climls
    }
    return(cls)
  }
  
  ncores <- detectCores()
  cl <- makeCluster(ncores, outfile = "")
  registerDoParallel(cl)
  
  print('Cluster Created')
  resls <- foreach(a = prls_combined, b = txls_combined, c = tnls_combined,
                   .packages=c('climdex.pcic'), .export = c('func_names','dts')) %dopar% {
                     climmer(a,b,c)
                   }
  stopCluster(cl)
  print('Cluster Stopped')
  saveRDS(resls,paste('./NEW/clims/zone_',z,'_p_',prd,'.rds',sep=""),compress = F)
  rm(resls) ; gc()
  print('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< EXITING LOOP >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>')
  print(proc.time()[3]-t1)
}



q()