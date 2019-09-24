args = commandArgs(trailingOnly=TRUE)
zone <- as.numeric(args[1])
dir.create(paste('./zone_',zone,sep=""))
print('<<<<<<<<<<<<<<<<<<<<< >>>>>>>>>>>>>>>>>>>>>>>>>>>')
print(zone)

library(climdex.pcic)
base_range <- c(1986,2015)
dts <- readRDS('RCM_Dates.rds')
dts <- as.PCICt(as.character(dts[13141:35040]), cal = '365')
func_names <- source('./clim_func_names.R')[[1]]
func_names <- func_names[c(1,7,14,17,21,22,24)]

prls <- readRDS(paste('./period_lists/',zone,'_pr_periods_list.rds',sep=""))
tmxls <- readRDS(paste('./period_lists/',zone,'_tmx_periods_list.rds',sep=""))
tmnls <- readRDS(paste('./period_lists/',zone,'_tmn_periods_list.rds',sep=""))

clim_wrapper <- function(pr, tmx, tmn, i) {
  # Each core receives a list of 5 periods
  tt1 <- proc.time()[3]
  print(paste("Run Number >>>>>>>>>>>",i))
  pr_mat_list <- list()
  tmx_mat_list <- list()
  tmn_mat_list <- list()
  
  for (prd in 1:5) {
    # These are a list of 30 (years) datraframes (365 * grids+1) # last column is year, remove it
    
    pr_mat <- as.matrix(do.call(rbind,pr[[prd]])[,-dim(pr[[prd]][[1]])[2]])
    tmx_mat <- as.matrix(do.call(rbind,tmx[[prd]])[,-dim(tmx[[prd]][[1]])[2]])
    tmn_mat <- as.matrix(do.call(rbind,tmn[[prd]])[,-dim(tmn[[prd]][[1]])[2]])
    # These are matrix of Rows = 365*30 and columns = number of grids
    pr_mat_list[[prd]] <- pr_mat
    tmx_mat_list[[prd]] <- tmx_mat
    tmn_mat_list[[prd]] <- tmn_mat
    rm(pr_mat,tmx_mat,tmn_mat);gc()
  }
  
  plist <- list()
  for (period in 2:5) {
    grid_list <- list()
    for (grid in 1:dim(pr_mat_list[[1]])[2]) {
      prec <- append(pr_mat_list[[1]][,grid], pr_mat_list[[period]][,grid])*86400
      tmax <- append(tmx_mat_list[[1]][,grid], tmx_mat_list[[period]][,grid])-273.15
      tmin <- append(tmn_mat_list[[1]][,grid], tmn_mat_list[[period]][,grid])-273.15
      ci <- climdexInput.raw(tmax = tmax, tmin = tmin, prec = prec, 
                             tmax.dates = dts, tmin.dates = dts, prec.dates = dts, 
                             base.range = base_range)
      grid_list[[length(grid_list)+1]] <- ci
      rm(prec,tmax,tmin,ci)
    }
    plist[[length(plist)+1]] <- grid_list
    rm(grid_list);gc()
  }
  
  saveRDS(plist,paste('./zone_',zone,'/r',i,'.rds',sep=""),compress = F)
  print(paste("Time Taken >>>>>>>>>>>",proc.time()[3]-tt1))
}

sapply(1:50, function(i) clim_wrapper(prls[[i]], tmxls[[i]], tmnls[[i]], i))

ccc <- function(k) {
  plist <- readRDS(paste('./zone_',zone,'/r',k,'.rds',sep=""))
  clim_prd_list <- list()
  for (prd in 1:length(plist)) {
    prdls <- plist[[prd]]
    grid_list <- list()
    for (grid in 1:length(prdls)) {
      resmat <- matrix(NA, nrow = 60, ncol = length(func_names))
      for (i in 1:length(func_names)) {
        resmat[,i] <- eval(parse(text = func_names[i]))(prdls[[grid]])
      }
      grid_list[[length(grid_list)+1]] <- resmat
    }
    clim_prd_list[[length(clim_prd_list)+1]] <- grid_list
    rm(grid_list); gc()
  }
  
  glist <- list()
  for (i in 1:length(clim_prd_list[[1]])) {
    mat <- rbind(clim_prd_list[[1]][[i]],
                 clim_prd_list[[2]][[i]][31:60,],
                 clim_prd_list[[3]][[i]][31:60,],
                 clim_prd_list[[4]][[i]][31:60,])
    glist[[length(glist)+1]] <- mat
  }
  return(glist)
}

ncores <- 25 ; print('Cored detected')
cl <- makeCluster(ncores) ; print('Cluster made')
registerDoParallel(cl) ; print('Cluster registered')
print(paste('Number of cores running is ', ncores, sep = ""))

print('Starting foreach')
resls <- foreach(i = seq_along(1:50), 
                 .packages=c('climdex.pcic'), .export = c('zone')) %dopar% {
                   ccc(i)
                 }
stopCluster(cl)


saveRDS(resls,paste('./Clim_result_final_',zone,'.rds',sep=""),compress = F)
q()