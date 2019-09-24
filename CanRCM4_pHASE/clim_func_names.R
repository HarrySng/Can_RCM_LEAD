library(climdex.pcic)

create_func_names <- function(..) { # Create a vector of climdex functions
  tmax.dates <- as.PCICt(do.call(paste, ec.1018935.tmax[,c("year",
                                                           "jday")]), format="%Y %j", cal="gregorian")
  tmin.dates <- as.PCICt(do.call(paste, ec.1018935.tmin[,c("year",
                                                           "jday")]), format="%Y %j", cal="gregorian")
  prec.dates <- as.PCICt(do.call(paste, ec.1018935.prec[,c("year",
                                                           "jday")]), format="%Y %j", cal="gregorian")
  ci <- climdexInput.raw(ec.1018935.tmax$MAX_TEMP,
                         ec.1018935.tmin$MIN_TEMP, ec.1018935.prec$ONE_DAY_PRECIPITATION,
                         tmax.dates, tmin.dates, prec.dates, base.range=c(1971, 2000))
  return(climdex.get.available.indices(ci))
}

func_names <- create_func_names(..)