library(rstan)
library(lubridate)
library(tidyverse)
library(reshape2)

fls <- paste('./onerun/Run',seq(1,50,1),'.rds',sep="")
lss <- lapply(fls, readRDS) 
dts <- readRDS('D:\\paper3\\stuff\\RCM_Dates.rds')
obs <- readRDS('D:\\PAPER2\\raw_RDS\\ansp_rgns.rds') # Anusplin

zone_id <- 2
base_period <- c(1981,2010)
fut_period <- c(2081,2100)

ff <- function(x) {
  df <- data.frame(x[[zone_id]]) # Extract zone
  df <- data.frame(rowMeans(df, na.rm = T)) # Take spatial average
  df$dt <- seq(as.Date('1950-01-01'), as.Date('2010-12-31'),1)
  df$dt <- year(df$dt)
  df <- df %>% filter(dt >= base_period[1] & dt <= base_period[2]) # Filter base period
  return(df)
}
obs <- lapply(obs, ff) # List of 3 elements (pr, tmx, tmn)

# Prepare data for stan input
# Take zone2, all 50 runs, averaged in a past and a future period

data_maker_stan <- function(z) {
  zone_list <- list()
  for (i in 1:50) {
    zone_list[[i]] <- lss[[i]][[z]] # Extract zone, all 50 runs
  }
  
  average_data <- function(r) { # Filter base and future periods
    r$yr <- year(dts)
    p1 <- r %>% filter(yr >= base_period[1] & yr <= base_period[2]) %>% mutate(yr = NULL) %>% summarise_all(mean) %>% mutate (pd = 'past')
    p2 <- r %>% filter(yr >= fut_period[1] & yr <= fut_period[2]) %>% mutate(yr = NULL) %>% summarise_all(mean) %>% mutate (pd = 'fut')
    return(rbind(p1,p2))
  }
  
  avg_data <- do.call('rbind',lapply(zone_list, average_data))
  avg_data$pr <- avg_data$pr*86400 # Change units
  avg_data$tmx <- avg_data$tmx-273.15
  avg_data$tmn <- avg_data$tmn-273.15
  past <- avg_data %>% filter(pd == 'past')
  fut <- avg_data %>% filter(pd == 'fut')
  
  pr_list <-list( # Create stan ready lists
    N = 50,
    obs = mean(obs[[1]]$rowMeans.df..na.rm...T.),
    past = past$pr,
    fut = fut$pr
  )
  tmx_list <-list(
    N = 50,
    obs = mean(obs[[2]]$rowMeans.df..na.rm...T.),
    past = past$tmx,
    fut = fut$tmx
  )
  tmn_list <-list(
    N = 50,
    obs = mean(obs[[3]]$rowMeans.df..na.rm...T.),
    past = past$tmn,
    fut = fut$tmn
  )
  return(list(pr_list,tmx_list,tmn_list))
}

stan_data_list <- data_maker_stan(zone_id) # List of 3 (pr, tmx, tmn) in stan list format


iter <- 2000 # number of MCMC samples
warmup = floor(iter/2) # Discard first half of samples
chains = 1 # Number of parallel chains

# fit the stan model
fit <- stan(file = './Scripts//stan_rcm_t.stan', data = stan_data_list[[2]],
            chains = chains, iter = iter, warmup = warmup, verbose = T) 

pars <- extract(fit, permuted = TRUE) # return parameters

# Create distributions
df <- data.frame(
  'past' = rnorm(1000,median(pars$mu0), (1/median(pars$lambda1))),
  'fut' = rnorm(1000,median(pars$mu2),(1/(median(pars$lambda2)*median(pars$theta))))
)

# all runs to plot on x axis for comparison
df2 <- data.frame(
  'past' = stan_data_list[[2]]$past,
  'fut' = stan_data_list[[2]]$fut
)

## See past uncertainty
ggplot() +
  geom_density(data = df, aes(x = past)) + 
  geom_point(data = df2, aes(x = past, y = 0)) +
  theme_bw()

## See future uncertainty
ggplot() +
  geom_density(data = df, aes(x = fut)) + 
  geom_point(data = df2, aes(x = fut, y = 0)) +
  theme_bw()

# see temp change based on posteriors
median(pars$mu2) - median(pars$mu0)
