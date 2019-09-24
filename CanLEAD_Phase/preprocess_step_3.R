# Calculates metrics on validation data created in step 2

library(Metrics)
library(lubridate)
library(tidyverse)
library(doParallel)

gd <- readRDS('./canlead_zone_ind.rds')
gd <- do.call(rbind,gd)
args = commandArgs(trailingOnly=TRUE)
vv <- args[1]

val_wrapper <- function(mns, var) {
  vls <- readRDS(paste('./NEW/',var,'_val.rds',sep=""))
  zls <- list()
  for (i in 1:15) {
    print(i)
    ls <- vls[[i]]
    
    validate <- function(m) {
      m <- data.frame(m)
      colnames(m) <- c('a','b','c','d')
      m$date <- seq(as.Date('1950-01-01'), as.Date('2005-12-31'),1)
      m <- m %>% filter(month(date) %in% mns) %>% group_by(year(date)) %>% summarise(a = mean(a), b = mean(b), c = mean(c), d = mean(d))
      m <- as.matrix(m[,c('a','b','c','d')])
      m <- na.omit(m)
      if(dim(m)[1] < 1000) {
        return(rep(NA,9))
      }
      if (var == 'pr') {
        m[,2] <- m[,2]*86400
        m[,3] <- m[,3]*86400
        m[,4] <- m[,4]*86400
      } else {
        m[,2] <- m[,2]-273.15
        m[,3] <- m[,3]-273.15
        m[,4] <- m[,4]-273.15
      }
      
      b1 <- bias(m[,1],m[,2])
      b2 <- bias(m[,1],m[,3])
      b3 <- bias(m[,1],m[,4])
      
      m1 <- mae(m[,1],m[,2])
      m2 <- mae(m[,1],m[,3])
      m3 <- mae(m[,1],m[,4])
      
      r1 <- rmse(m[,1],m[,2])
      r2 <- rmse(m[,1],m[,3])
      r3 <- rmse(m[,1],m[,4])
      
      res <- c(b1,b2,b3,m1,m2,m3,r1,r2,r3)
      return(res)
    }
    
    ncores <- 80
    cl <- makeCluster(ncores, outfile = "")
    registerDoParallel(cl)
    
    resls <- foreach(mat = ls,
                     .packages=c('Metrics','lubridate','tidyverse'), .export = c()) %dopar% {
                       validate(mat)
                     }
    
    stopCluster(cl)
    
    zls[[i]] <- resls
  }
  
  for (i in 1:15) {
    zls[[i]] <- do.call(rbind,zls[[i]])
  }
  ddf <- data.frame(do.call(rbind,zls))
  ddf$lon <- gd$lon ; ddf$lat <- gd$lat
  return(ddf)
}

mnsls <- list(c(12,1,2), c(3,4,5), c(6,7,8), c(9,10,11))
rls <- lapply(mnsls, val_wrapper, vv)
saveRDS(rls,paste('./NEW/',vv,'_valres.rds',sep=""),compress=F)
q()
