library(lubridate)
library(tidyverse)
library(climdex.pcic)

source('clim_func_names.R')
func_names <- func_names[c(3,4,5,6,12,13,7,16,19,21,22,24,25,26)]
base_range <- c(1976,2005)

prls <- readRDS('./NEW/pr_val.rds')
txls <- readRDS('./NEW/tasmax_val.rds')
tnls <- readRDS('./NEW/tasmin_val.rds')

for (i in 1:15) {
  ls1 <- prls[[i]]
  ls2 <- txls[[i]]
  ls3 <- tnls[[i]]
  print(i)
  print('Starting wrapper')
  clim_wrapper <- function(pr, tx, tn) {
    #
    tn <- cbind(tn, tn[,3]) ; tx <- cbind(tx, tx[,3]) ; pr <- cbind(pr, pr[,3])
    #
    
    pr[,2:4] <- pr[,2:4]*86400
    tx[,2:4] <- tx[,2:4]-273.15
    tn[,2:4] <- tn[,2:4]-273.15
    
    subsetter <- function(m) {
      m <- data.frame(m)
      m$date <- seq(as.Date('1950-01-01'), as.Date('2005-12-31'),1)
      m <- m %>% filter(year(date) >= 1976 & year(date) <= 2005)
      return(as.matrix(m[,1:4]))
    }
    pr <- subsetter(pr) ; tx <- subsetter(tx) ; tn <- subsetter(tn)
    
    dts <- seq(as.Date('1976-01-01'), as.Date('2005-12-31'),1)
    dts <- as.PCICt(as.character(dts), cal = '365')
    
    cls <- list()
    for (d in 1:4) {
      ci <- climdexInput.raw(tmax = tx[,d], tmin = tn[,d], prec = pr[,d], 
                             tmax.dates = dts, tmin.dates = dts, prec.dates = dts, 
                             base.range = base_range)
      climls <- list()
      for (i in 1:length(func_names)) {
        climls[[i]] <- eval(parse(text = func_names[i]))(ci)
      }
      cls[[d]] <- climls
    }
    return(cls)
  }
  
  ncores <- detectCores()
  cl <- makeCluster(ncores, outfile = "")
  registerDoParallel(cl)
  
  print('Cluster Created')
  resls <- foreach(a = ls1, b = ls2, c = ls3,
                   .packages=c('climdex.pcic','tidyverse','lubridate'), .export = c('func_names','base_range')) %dopar% {
                     clim_wrapper(a,b,c)
                   }
  
  stopCluster(cl)
  print('Cluster Stopped')
  
  saveRDS(resls,paste('./NEW/hist_clim/hc_zone_',i,'.rds',sep=""),compress = F)
  rm(resls)
}

q()