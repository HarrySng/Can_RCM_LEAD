lss <- lapply(paste('./canlead/ress/Run_',seq(1,20,1),'.rds',sep=""),readRDS)

resls <- list()
for (i in 1:15) {
  ls <- list()
  for (j in 1:20) {
    ls[[j]] <- lss[[j]][[i]]
  }
  resls[[i]] <- Reduce(`+`, ls) / length(ls)
}

dts <- readRDS('./rds_obj/RCM_Dates.rds')

genp <- function(df) {
  tmp <- ((df$tmx+df$tmn)/2)-273.15
  tmp <- data.frame(tmp)
  tmp$yr <- year(dts)
  tmp_ls <- split(tmp, tmp$yr)
  
  base_period <- tmp_ls[37:66]
  base_mean <- colMeans(do.call('rbind', base_period))[1]
  fut_period <- tmp_ls[37:151]
  running_mean <- function(..) {
    mean_vec <- c()
    for (i in 1:(length(fut_period)-29)) {
      fut_df <- do.call('rbind', fut_period[i:(i+29)])
      fut_mean <- colMeans(fut_df)[1]
      mean_vec <- append(mean_vec, fut_mean - base_mean)
    }
    return(mean_vec)
  }
  fmean <- running_mean(..)
  return(fmean)
}

fmeans <- lapply(resls, genp)
saveRDS(fmeans,'./canlead/canlead_moving_window_means.rds',compress = F)
