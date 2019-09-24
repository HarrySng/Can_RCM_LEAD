# Makes validation data

obs <- readRDS('./NEW/ansp_up.rds')
ogg <- readRDS('./NEW/orig_up.rds')
cl1 <- readRDS('./NEW/cld1_up.rds')
cl2 <- readRDS('./NEW/cld2_up.rds')

print('aaaaaaaaaaa')
zls <- list()
for (i in 1:15) {
  lss <- list()
  for (g in 1:dim(obs[[1]][[i]])[2]) {
    lss[[g]] <- cbind(obs[[1]][[i]][,g], ogg[[1]][[i]][1:20454,g], cl1[[1]][[i]][1:20454,g], cl2[[i]][[1]][1:20454,g])
  }
  zls[[i]] <- lss
}
saveRDS(zls,'./NEW/pr_val.rds',compress = F)
rm(zls,lss)
gc()

print('bbbbbbbbbb')

zls <- list()
for (i in 1:15) {
  lss <- list()
  for (g in 1:dim(obs[[2]][[i]])[2]) {
    lss[[g]] <- cbind(obs[[2]][[i]][,g], ogg[[2]][[i]][1:20454,g], cl1[[2]][[i]][1:20454,g], cl2[[i]][[2]][1:20454,g])
  }
  zls[[i]] <- lss
}
saveRDS(zls,'./NEW/tasmax_val.rds',compress = F)
rm(zls,lss)
gc()

print('cccccccccc')
zls <- list()
for (i in 1:15) {
  lss <- list()
  for (g in 1:dim(obs[[3]][[i]])[2]) {
    lss[[g]] <- cbind(obs[[3]][[i]][,g], ogg[[3]][[i]][1:20454,g], cl1[[3]][[i]][1:20454,g], cl2[[i]][[3]][1:20454,g])
  }
  zls[[i]] <- lss
}
saveRDS(zls,'./NEW/tasmin_val.rds',compress = F)
rm(zls,lss)
gc()
q()