library(rstan)
library(lubridate)
library(tidyverse)
library(reshape2)
library(doParallel)

################### Parameters
dts <- readRDS('D:\\paper3\\stuff\\RCM_Dates.rds')
base_period <- c(1951,2010)
####################################

parent_wrapper <- function(season) {
  ####################################
  ssn_func <- function(ssn) {
    if(ssn == 'Spring') {return(c(3,4,5))}
    else if(ssn == 'Summer') {return(c(6,7,8))}
    else if(ssn == 'Fall') {return(c(9,10,11))}
    else if(ssn == 'Winter') {return(c(12,1,2))}
  }
  ssn <- ssn_func(season)
  decadal_means <- function(df) {
    lss <- split(df,year(df$date))
    dm <- list()
    for (i in seq(1,length(lss),10)) {
      dm[[length(dm)+1]] <- colMeans(do.call('rbind', lss[i:(i+9)])[,1:2])
    }
    return(data.frame(do.call('rbind',dm)))
  }
  ####################################
  
  stan_data_maker <- function(ssn) {
    obs <- readRDS('./HB/obs_stan_data.rds')
    
    subsetter <- function(d) {
      d$date <- seq(as.Date('1950-01-01'), as.Date('2010-12-31'),1)
      leap_years <- year(d$date[which(month(d$date)==2 & day(d$date)==29)])
      d <- d[-which(year(d$date) %in% leap_years & month(d$date)==11 & day(d$date)==30),] # remove 30Nov of leap years
      d <- d %>% filter(year(date) >= base_period[1] & year(date) <= base_period[2])
      d <- d %>% filter(month(date) %in% ssn)
      d <- decadal_means(d)
      return(d)
    }
    obs <- subsetter(obs)
    
    subsetter2 <- function(d) {
      d$date <- dts
      d <- d %>% filter(month(date) %in% ssn)
      d <- d %>% filter(year(date) >= base_period[1])
      d <- decadal_means(d)
    }
    crc4 <- lapply(readRDS('./HB/crc4_stan_data.rds'), subsetter2)
    cld1 <- lapply(readRDS('./HB/cld1_stan_data.rds'), subsetter2)
    cld2 <- lapply(readRDS('./HB/cld2_stan_data.rds'), subsetter2)
    
    pmat <- tmat <- matrix(NA, nrow = 15, ncol = 150)
    for (i in 1:50) {
      pmat[,i] <- crc4[[i]][,1] ; tmat[,i] <- crc4[[i]][,2]
      pmat[,i+50] <- cld1[[i]][,1] ; tmat[,i+50] <- cld1[[i]][,2]
      pmat[,i+100] <- cld2[[i]][,1] ; tmat[,i+100] <- cld2[[i]][,2]
    }
    
    stan_list <-list( # Create stan ready lists
      N = 6,
      DCD = 15,
      MDL = 150,
      t_obs = obs$tmp,
      p_obs = log(obs$pr),
      t_mdl = tmat,
      p_mdl = log(pmat)
    )
    
    return(stan_list)
  }
  
  stan_data <- stan_data_maker(ssn)
  
  iter <- 2000 # number of MCMC samples
  warmup = floor(iter/2) # Discard first half of samples
  chains = 1 # Number of parallel chains
  
  fit <- stan(file = './HB/HB_CanLEAD.stan', data = stan_data,
              chains = chains, iter = iter, warmup = warmup, verbose = T, cores = 1) 
  
  saveRDS(fit,paste('./HB/fit_',season,'.rds',sep=""),compress = F)
}

ssns <- c('Winter','Spring','Summer','Fall')
ncores <- 4 ;
cl <- makeCluster(ncores)
registerDoParallel(cl)

print('Starting foreach')
foreach(s = ssns, 
        .packages=c('rstan','lubridate','tidyverse','reshape2'), .export = c('dts','base_period')) %dopar% {
                   parent_wrapper(s)
                 }
stopCluster(cl)

q()

