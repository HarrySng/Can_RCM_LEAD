library(lubridate)
library(doParallel)


## Set parameters
nms <- paste('./rds_obj/onerun/Run',seq(1,50,1),'.rds',sep="") # File names vector
base_period <- c(1986,2015)
yr_vec <- rep(seq(1950,2100,1),each = 365)
base_index <- c(
  which(seq(1950,2100,1) == base_period[1]),
  which(seq(1950,2100,1) == base_period[2])
)

manual_clim_wrapper <- function(fname) { # Calculate climdex manually
  
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
    
    # Calculate tmp quantiles
    #tmn10 <- quantile(base_df$tmn,0.1) - 273.15
    #tmn90 <- quantile(base_df$tmn,0.9) - 273.15 
    #tmx10 <- quantile(base_df$tmx,0.1) - 273.15 
    #tmx90 <- quantile(base_df$tmx,0.9) - 273.15
    tmx90 <- 32 # Heat wave 
    
    # Calculate pr quantiles
    pr95 <- quantile(base_df$pr,0.95)*86400
    pr99 <- quantile(base_df$pr,0.99)*86400
    
    # Write a host of functions to apply to yearly lists - These are variants of functions in library(climdex.pcic)

    clim.fd <- function(x) {return(which((x$tmn-273.15) < 0))} # Frost days
    clim.su <- function(x) {return(which((x$tmx-273.15) > 25))} # Summer days
    clim.id <- function(x) {return(which((x$tmx-273.15) < 0))} # Icing days
    clim.tr <- function(x) {return(which((x$tmn-273.15) > 20))} # Tropical Nights
    
    clim.tmn10 <- function(x) {return(which((x$tmn-273.15) < tmn10))} # All days with tmn < 10th quantile - Lower tail of cold days
    clim.tmn90 <- function(x) {return(which((x$tmn-273.15) > tmn90))} # All days with tmn > 90th quantile - Upper tail of cold days
    clim.tmx10 <- function(x) {return(which((x$tmx-273.15) < tmx10))} # All days with tmx < 10th quantile - Lower tail of hot days
    clim.tmx90 <- function(x) {return(which((x$tmx-273.15) > tmx90))} # All days with tmn > 10th quantile - Upper tail of hot days
    
    clim.wd <- function(x) {return(which((x$pr*86400) > 1))} # Wet Days
    clim.r10 <- function(x) {return(which((x$pr*86400) > 10))} # Days when pcp > 10mm
    clim.r20 <- function(x) {return(which((x$pr*86400) > 20))} # Days when pcp > 20mm
    clim.pr95 <- function(x) {return(which((x$pr*86400) > pr95))} # All days with pr > 95th quantile
    clim.pr99 <- function(x) {return(which((x$pr*86400) > pr99))} # All days with pr > 99th quantile
    
    # Duration Spell functions
    
    # Algorithm that fetches indices of the duration beginning
    # vec <- a vector of indices where data satisfies a condition
    # cons_days <- consecutive days threshold
    
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
      ind <- duration_algo(ndays,6)
      if (is.null(ind)) {
        return(NA)
      } else {
          return(ndays[ind])
        }
    }
    
    # Cold spell duration
    clim.csdi <- function(x) {
      ndays <- clim.tmn10(x)
      ind <- duration_algo(ndays,6)
      if (is.null(ind)) {
        return(NA)
      } else {
        return(ndays[ind])
      }
    }
    
    clim.dd <- function(x) {return(which((x$pr*86400) > 1))} # Dry Days
    # Consecutive Dry Days
    clim.cdd <- function(x) {
      ndays <- clim.dd(x)
      ind <- duration_algo(ndays,6)
      if (is.null(ind)) {
        return(NA)
      } else {
        return(ndays[ind])
      }
    }
    
    # Consecutive Wet Days
    clim.cwd <- function(x) {
      ndays <- clim.wd(x)
      ind <- duration_algo(ndays,6)
      if (is.null(ind)) {
        return(NA)
      } else {
        return(ndays[ind])
      }
    }
    
    data_holder[[length(data_holder)+1]] <- list(
      lapply(yr_list,clim.fd), # 1- Frost Days
      lapply(yr_list,clim.su), # 2- Summer Days
      lapply(yr_list,clim.id), # 3- icing Days
      lapply(yr_list,clim.tr), # 4- Tropical Nights
      lapply(yr_list,clim.tmn10), # 5- Lower Tail Cold Days
      lapply(yr_list,clim.tmn90), # 6- Upper Tail Cold Days
      lapply(yr_list,clim.tmx10), # 7- Lower Tail Warm Days
      lapply(yr_list,clim.tmx90), # 8-Upper Tail Warm Days
      lapply(yr_list,clim.wd), # 9- Wet Days
      lapply(yr_list,clim.r10), # 10- Pr > 10
      lapply(yr_list,clim.r20), # 11- Pr > 20
      lapply(yr_list,clim.pr95), # 12- 95th Extreme
      lapply(yr_list,clim.pr99), # 13- 99th Extreme
      lapply(yr_list,clim.wsdi), # 14- Warm Spell
      lapply(yr_list,clim.csdi), # 15- Cold Spell
      lapply(yr_list,clim.cdd), # 16- Dry Spell
      lapply(yr_list,clim.cwd) # 17- Wet Spell
    )
  }  ### Loop ends here
  saveRDS(data_holder,paste('./overlap/',substr(fname,10,nchar(fname)),sep=""),compress = F)
}

# Initiate 50 workers (one for each file)
ncores <- 50 
cl <- makeCluster(ncores, outfile = "")
registerDoParallel(cl)
print(paste('Number of cores running is ', ncores, sep = ""))

foreach(ipar = seq_along(nms),
        .packages=c('lubridate'), .export = c('manual_clim_wrapper','base_index','yr_vec')) %dopar% {
          manual_clim_wrapper(nms[ipar])
        }


stopCluster(cl)
