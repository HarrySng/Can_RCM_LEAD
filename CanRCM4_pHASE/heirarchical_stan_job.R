library(rstan)
library(lubridate)
library(tidyverse)
library(reshape2)
library(doParallel)

################### Parameters


################## Set up observations


ff <- function(zone_id) {
  dts <- readRDS('D:\\paper3\\stuff\\RCM_Dates.rds')
  base_period <- c(1951,2010)
  season <- 'Winter'
  
  ssn_func <- function(ssn) {
    if(ssn == 'Spring') {return(c(3,4,5))}
    else if(ssn == 'Summer') {return(c(6,7,8))}
    else if(ssn == 'Fall') {return(c(9,10,11))}
    else if(ssn == 'Winter') {return(c(12,1,2))}
  }
  
  ssn <- ssn_func(season)
  make_obs <- function(obs) {
    obs <- data.frame(
      'dt' = seq(as.Date('1950-01-01'), as.Date('2010-12-31'),1),
      'pr' = rowMeans(obs[[1]][[zone_id]],na.rm=T),
      'tmx' = rowMeans(obs[[2]][[zone_id]],na.rm=T),
      'tmn' = rowMeans(obs[[3]][[zone_id]],na.rm=T)
    )
    leap_years <- year(obs$dt[which(month(obs$dt)==2 & day(obs$dt)==29)])
    obs <- obs[-which(year(obs$dt) %in% leap_years & month(obs$dt)==11 & day(obs$dt)==30),] # remove 30Nov of leap years
    obs <- obs %>% filter(year(dt) >= base_period[1] & year(dt) <= base_period[2])
    obs <- obs %>% filter(month(dt) %in% ssn)
    obs$tmp <- (obs$tmx+obs$tmn)/2
    obs$tmx <- obs$tmn <- NULL
    decadal_means <- function(obs) {
      lss <- split(obs,year(obs$dt))
      dm <- list()
      for (i in seq(1,length(lss),10)) {
        dm[[length(dm)+1]] <- colMeans(do.call('rbind', lss[i:(i+9)])[,2:3])
      }
      return(data.frame(do.call('rbind',dm)))
    }
    obs <- decadal_means(obs)
    colnames(obs) <- c('pr','tmp')
    return(obs)
  }
  obs <- data.frame(make_obs(readRDS('D:\\PAPER2\\raw_RDS\\ansp_rgns.rds')))
  
  ################## Set up RCM data
  
  data_maker_stan <- function(zone_id) {
    fls <- paste('./rds_obj/onerun/Run',seq(1,50,1),'.rds',sep="")
    lss <- lapply(fls, readRDS) 
    zone_list <- list()
    for (i in 1:50) {
      zone_list[[i]] <- lss[[i]][[zone_id]] # Extract zone, all 50 runs
    }
    
    average_data <- function(r) { # Filter base and future periods
      r$dt <- dts
      r$tmp <- (r$tmx+r$tmn)/2
      r$tmx <- r$tmn <- NULL
      r$pr <- r$pr*86400
      r$tmp <- r$tmp-273.15
      r <- r %>% filter(month(dt) %in% ssn)
      r <- r %>% filter(year(dt) >= base_period[1])
      decadal_means <- function(r) {
        lss <- split(r,year(r$dt))
        dm <- list()
        for (i in seq(1,length(lss),10)) {
          dm[[length(dm)+1]] <- colMeans(do.call('rbind', lss[i:(i+9)])[,c(1,3)])
        }
        return(data.frame(do.call('rbind',dm)))
      }
      r <- decadal_means(r)
      colnames(obs) <- c('pr','tmp')
      return(data.frame(r))
    }
    
    avg_data <- lapply(zone_list, average_data)
    pmat <- tmat <- matrix(nrow = 15, ncol = 50)
    for (i in 1:50) {
      pmat[,i] <- avg_data[[i]][,1]
      tmat[,i] <- avg_data[[i]][,2]
    }
    
    stan_list <-list( # Create stan ready lists
      N = 6,
      DCD = 15,
      MDL = 50,
      t_obs = obs$tmp,
      p_obs = log(obs$pr),
      t_mdl = tmat,
      p_mdl = log(pmat)
    )
    return(stan_list)
  }
  
  stan_data_list <- data_maker_stan(zone_id) # List of 3 (pr, tmx, tmn) in stan list format
  
  iter <- 2000 # number of MCMC samples
  warmup = floor(iter/2) # Discard first half of samples
  chains = 1 # Number of parallel chains
  
  fit <- stan(file = paste('./Scripts/hh/Heirarchical_Model_',zone_id,'.stan',sep=""), data = stan_data_list,
              chains = chains, iter = iter, warmup = warmup, verbose = T, cores = 1) 
  
  saveRDS(fit,paste("./Stan_Models/stan_fit_",zone_id,'.rds',sep=""),compress = F)
  
}

ncores <- 5 ; print('Cored detected')
cl <- makeCluster(ncores) ; print('Cluster made')
registerDoParallel(cl) ; print('Cluster registered')
print(paste('Number of cores running is ', ncores, sep = ""))

print('Starting foreach')
resls <- foreach(i = 1:15, 
                 .packages=c('tidyverse','rstan','lubridate','reshape2'), .export = c()) %dopar% {
                   ff(i)
                 }
stopCluster(cl)

q()





