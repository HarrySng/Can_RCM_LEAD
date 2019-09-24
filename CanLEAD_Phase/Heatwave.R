library(lubridate)
library(doParallel)

rr <- 20
## Set parameters
nms1 <- paste('./rds_obj/onerun/Run',seq(1,rr,1),'.rds',sep="") # File names vector
nms2 <- paste('./canlead/ress/Run_',seq(1,rr,1),'.rds',sep="") # File names vector
base_period <- c(1986,2015)
yr_vec <- rep(seq(1950,2100,1),each = 365)
base_index <- c(
  which(seq(1950,2100,1) == base_period[1]),
  which(seq(1950,2100,1) == base_period[2])
)
mean_diff1 <- readRDS('./rds_obj/moving_window_means.rds')
mean_diff2 <- readRDS('./canlead/canlead_moving_window_means.rds')
dfp <- readRDS('./rds_obj/final_pop.rds')

manual_clim_wrapper <- function(fname, md) { # Calculate climdex manually
  
  # Create a year vector to split data annually
  
  # Read data
  zone_list <- readRDS(fname) 
  
  # Create a place holder
  data_holder <- list()
  
  # Iterate over zone list
  for (i in 1:15) {
    
    # Split data by years
    yr_list <- split(zone_list[[i]], yr_vec)
    
    # Need to change rownames as splitting keeps the old index
    yr_list = lapply(yr_list, function(x) {
      rownames(x) <- seq(1,365,1)
      return(x)
    })
    
    # Create a separate dataset for base period
    base_df <- do.call('rbind',yr_list[base_index[1]:base_index[2]])

    tmx90 <- 32 # Heat wave 
    clim.tmx90 <- function(x) {return(which((x$tmx-273.15) > tmx90))} # All days with tmn > 10th quantile - Upper tail of hot days
    
    duration_algo <- function(vec, cons_days) {
      res <- c()
      if (length(vec) > cons_days) {
        n <- length(vec)
        i <- 1 ; j <- cons_days
        while(j <= n) {
          if (vec[j] - vec[i] == (cons_days-1)) {
            res <- append(res,i)
            i = j + 1 ; j = i + (cons_days-1)
          } else{
            i = i + 1 ; j = j + 1
          }
        }
        return(res)
      } else {
        return(NA)
      }
    }
    
    # Warm spell duration
    clim.wsdi <- function(x) {
      ndays <- clim.tmx90(x)
      ind <- duration_algo(ndays,3)
      if (is.null(ind)) {
        return(NA)
      } else {
        return(ndays[ind])
      }
    }
    
    resls <- lapply(yr_list,clim.wsdi)
    
    inds <- c()
    for (t in c(0.5,1,2,3)) {
      inds <- append(inds, which(md[[i]] > t)[1])
    }
    
    heatwaves <- c(
      length(which(!is.na(unlist(resls[37:66])))),
      length(which(!is.na(unlist(resls[(inds[1]+37):(inds[1]+37+29)])))),
      length(which(!is.na(unlist(resls[(inds[2]+37):(inds[2]+37+29)])))),
      length(which(!is.na(unlist(resls[(inds[3]+37):(inds[3]+37+29)])))),
      length(which(!is.na(unlist(resls[(inds[4]+37):(inds[4]+37+29)]))))
    )
    
    expo <- c(
      heatwaves[1] * dfp$pop1[i],
      heatwaves[2] * dfp$pop1[i],
      heatwaves[3] * dfp$pop1[i],
      heatwaves[4] * dfp$pop1[i],
      heatwaves[5] * dfp$pop1[i]
    )
    data_holder[[i]] <- heatwaves #############################################
  }  ### Loop ends here
  return(data_holder)
}

# Initiate 50 workers (one for each file)
ncores <- 5
cl <- makeCluster(ncores, outfile = "")
registerDoParallel(cl)
print(paste('Number of cores running is ', ncores, sep = ""))

resls1 <- foreach(ipar = seq_along(nms1),
        .packages=c('lubridate'), .export = c('manual_clim_wrapper','base_index','yr_vec','dfp')) %dopar% {
          manual_clim_wrapper(nms1[ipar], mean_diff1)
        }

resls2 <- foreach(ipar = seq_along(nms2),
                  .packages=c('lubridate'), .export = c('manual_clim_wrapper','base_index','yr_vec','dfp')) %dopar% {
                    manual_clim_wrapper(nms2[ipar],mean_diff2)
                  }
stopCluster(cl)

rls1 <- list()
rls2 <- list()
for (i in 1:15) {
  lss1 <- list()
  lss2 <- list()
  for (j in 1:rr) {
    lss1[[j]] <- resls1[[j]][[i]]
    lss2[[j]] <- resls2[[j]][[i]]
  }
  rls1[[i]] <- lss1
  rls2[[i]] <- lss2
}

lss1 <- list()
lss2 <- list()
for (z in 1:15) {
  m1 <- do.call(rbind,rls1[[z]])
  m2 <- do.call(rbind,rls2[[z]])
  mm1 <- matrix(NA, nrow = 3, ncol = 5)
  mm2 <- matrix(NA, nrow = 3, ncol = 5)
  for (i in 1:5) {
    mm1[1,i] <- min(m1[2,i]) ; mm1[2,i] <- median(m1[,i]) ; mm1[3,i] <- max(m1[,i])   
    mm2[1,i] <- min(m2[2,i]) ; mm2[2,i] <- median(m2[,i]) ; mm2[3,i] <- max(m2[,i])  
  }
  lss1[[z]] <- mm1
  lss2[[z]] <- mm2
}

  
ddf1 <- data.frame(do.call(rbind,lss1)) ; ddf1$data <- 'CanRCM4'
ddf2 <- data.frame(do.call(rbind,lss2)) ; ddf2$data <- 'CanLEAD'
ddf <- rbind(ddf1,ddf2)

ddf$zone <- rep(rep(seq(1,15,1),each = 3),2)
ddf$id <- rep(rep(c('min','med','max'),15),2)  

# Only median for now
ddf <- ddf %>% filter(id == 'med')
dfplot <- melt(ddf,c('zone','id','data'))

z <- 9
ggplot() +
  geom_point(data = ddf %>% filter(data == 'CanRCM4') %>% filter(zone == z) %>% melt(c('zone','data','id')), 
             aes(x = variable, y = value), size = 3, color = 'red4') +
  geom_point(data = ddf %>% filter(data == 'CanLEAD') %>% filter(zone == z) %>% melt(c('zone','data','id')),
             aes(x = variable, y = value), size = 3, color = 'navyblue') + theme_bw()
