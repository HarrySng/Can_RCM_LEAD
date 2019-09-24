## Prep data for bayesian hierarchical
# 3 RCM models - 50 runs each. Treat them as separate 150 models
# Total spatial aggregation, no zones
# Seasonal aggregation

# Observation first
obsls <- readRDS('D:\\PAPER2\\raw_RDS\\ansp_rgns.rds')
for (i in 1:3) {
  bigm <- do.call(cbind,obsls[[i]])
  obsls[[i]] <- rowMeans(bigm, na.rm=T)
}
obs_df <- data.frame(pr = obsls[[1]], tmp = (obsls[[2]]+obsls[[3]])/2)
rm(list=setdiff(ls(), c('obs_df')))

# Now CanRCM4
fls <- paste('./rds_obj/onerun_orig/Run',seq(1,50,1),'.rds',sep="")
dls <- lapply(fls,readRDS)
sp_mean <- function(ls) {
  m1 <- m2 <- m3 <- list()
  for (i in 1:15) {
    m1[[i]] <- ls[[i]][,1]
    m2[[i]] <- ls[[i]][,2]
    m3[[i]] <- ls[[i]][,3]
  }
  m1 <- do.call(cbind,m1)
  m2 <- do.call(cbind,m2)
  m3 <- do.call(cbind,m3)
  rcm_df <- data.frame(pr = rowMeans(m1,na.rm=T)*86400, tmp = (rowMeans(m2,na.rm = T)+rowMeans(m3,na.rm = T))/2-273.15)
  return(rcm_df)
}
crc4 <- lapply(dls, sp_mean)
rm(list=setdiff(ls(), c('obs_df','crc4')))

# Now CanLEADS
sp_mean2 <- function(ls) {
  m1 <- m2 <- m3 <- list()
  for (i in 1:15) {
    m1[[i]] <- ls[[1]][[i]]
    m2[[i]] <- ls[[2]][[i]]
    m3[[i]] <- ls[[3]][[i]]
  }
  m1 <- do.call(cbind,m1)
  m2 <- do.call(cbind,m2)
  m3 <- do.call(cbind,m3)
  rcm_df <- data.frame(pr = rowMeans(m1,na.rm=T)*86400, tmp = (rowMeans(m2,na.rm = T)+rowMeans(m3,na.rm = T))/2-273.15)
  return(rcm_df)
}

cld1 <- lapply(lapply(paste('./rds_obj/onerun_cld1/Run',seq(1,50,1),'.rds',sep=""), readRDS), sp_mean2)
cld2 <- lapply(lapply(paste('./rds_obj/onerun_cld2/Run',seq(1,50,1),'.rds',sep=""), readRDS), sp_mean2)

saveRDS(obs_df,'./HB/obs_stan_data.rds', compress = F)
saveRDS(crc4,'./HB/crc4_stan_data.rds', compress = F)
saveRDS(cld1,'./HB/cld1_stan_data.rds', compress = F)
saveRDS(cld2,'./HB/cld2_stan_data.rds', compress = F)


