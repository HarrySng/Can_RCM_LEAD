# Make periods of warming

ogg <- readRDS('./NEW/orig_up.rds')
cl1 <- readRDS('./NEW/cld1_up.rds')
cl2 <- readRDS('./NEW/cld2_up.rds')
indls <- readRDS('./NEW/prds_ind_dates.rds')

z0 <- z1 <- z2 <- z3 <- z4 <- list()
for (i in 1:15) {
  print(i)
  i1 <- indls[[1]][i,] ; i2 <- indls[[2]][i,] ; i3 <- indls[[3]][i,] 
  p0 <- p1 <- p2 <- p3 <- p4 <- list()
  for (g in 1:dim(ogg[[1]][[i]])[2]) {
    p0[[g]] <- cbind(ogg[[1]][[i]][13141:24090,g], cl1[[1]][[i]][13141:24090,g], cl2[[i]][[1]][13141:24090,g])
    p1[[g]] <- cbind(ogg[[1]][[i]][(i1[1]):(i1[1]+10949),g], cl1[[1]][[i]][i2[1]:(i2[1]+10949),g], cl2[[i]][[1]][i3[1]:(i3[1]+10949),g])
    p2[[g]] <- cbind(ogg[[1]][[i]][(i1[2]):(i1[2]+10949),g], cl1[[1]][[i]][i2[2]:(i2[2]+10949),g], cl2[[i]][[1]][i3[2]:(i3[2]+10949),g])
    p3[[g]] <- cbind(ogg[[1]][[i]][(i1[3]):(i1[3]+10949),g], cl1[[1]][[i]][i2[3]:(i2[3]+10949),g], cl2[[i]][[1]][i3[3]:(i3[3]+10949),g])
    p4[[g]] <- cbind(ogg[[1]][[i]][(i1[4]):(i1[4]+10949),g], cl1[[1]][[i]][i2[4]:(i2[4]+10949),g], cl2[[i]][[1]][i3[4]:(i3[4]+10949),g])
  }
  z0[[i]] <- p0 ; z1[[i]] <- p1 ; z2[[i]] <- p2 ; z3[[i]] <- p3 ; z4[[i]] <- p4
  print('aaaaaaaaaaa')
  rm(p0,p1,p2,p3,p4) ; gc()
}

saveRDS(z0,'./NEW/prds/pr0.rds',compress = F)
saveRDS(z1,'./NEW/prds/pr1.rds',compress = F)
saveRDS(z2,'./NEW/prds/pr2.rds',compress = F)
saveRDS(z3,'./NEW/prds/pr3.rds',compress = F)
saveRDS(z4,'./NEW/prds/pr4.rds',compress = F)

rm(z0,z1,z2,z3,z4) ; gc()


z0 <- z1 <- z2 <- z3 <- z4 <- list()
for (i in 1:15) {
  print(i)
  i1 <- indls[[1]][i,] ; i2 <- indls[[2]][i,] ; i3 <- indls[[3]][i,] 
  p0 <- p1 <- p2 <- p3 <- p4 <- list()
  for (g in 1:dim(ogg[[2]][[i]])[2]) {
    p0[[g]] <- cbind(ogg[[2]][[i]][13141:24090,g], cl1[[2]][[i]][13141:24090,g], cl2[[i]][[2]][13141:24090,g])
    p1[[g]] <- cbind(ogg[[2]][[i]][(i1[1]):(i1[1]+10949),g], cl1[[2]][[i]][i2[1]:(i2[1]+10949),g], cl2[[i]][[2]][i3[1]:(i3[1]+10949),g])
    p2[[g]] <- cbind(ogg[[2]][[i]][(i1[2]):(i1[2]+10949),g], cl1[[2]][[i]][i2[2]:(i2[2]+10949),g], cl2[[i]][[2]][i3[2]:(i3[2]+10949),g])
    p3[[g]] <- cbind(ogg[[2]][[i]][(i1[3]):(i1[3]+10949),g], cl1[[2]][[i]][i2[3]:(i2[3]+10949),g], cl2[[i]][[2]][i3[3]:(i3[3]+10949),g])
    p4[[g]] <- cbind(ogg[[2]][[i]][(i1[4]):(i1[4]+10949),g], cl1[[2]][[i]][i2[4]:(i2[4]+10949),g], cl2[[i]][[2]][i3[4]:(i3[4]+10949),g])
  }
  z0[[i]] <- p0 ; z1[[i]] <- p1 ; z2[[i]] <- p2 ; z3[[i]] <- p3 ; z4[[i]] <- p4
  print('bbbbbbbbbbb')
  rm(p0,p1,p2,p3,p4) ; gc()
}

saveRDS(z0,'./NEW/prds/tasmax0.rds',compress = F)
saveRDS(z1,'./NEW/prds/tasmax1.rds',compress = F)
saveRDS(z2,'./NEW/prds/tasmax2.rds',compress = F)
saveRDS(z3,'./NEW/prds/tasmax3.rds',compress = F)
saveRDS(z4,'./NEW/prds/tasmax4.rds',compress = F)

rm(z0,z1,z2,z3,z4) ; gc()

z0 <- z1 <- z2 <- z3 <- z4 <- list()
for (i in 1:15) {
  print(i)
  i1 <- indls[[1]][i,] ; i2 <- indls[[2]][i,] ; i3 <- indls[[3]][i,] 
  p0 <- p1 <- p2 <- p3 <- p4 <- list()
  for (g in 1:dim(ogg[[3]][[i]])[2]) {
    p0[[g]] <- cbind(ogg[[3]][[i]][13141:24090,g], cl1[[3]][[i]][13141:24090,g], cl2[[i]][[3]][13141:24090,g])
    p1[[g]] <- cbind(ogg[[3]][[i]][(i1[1]):(i1[1]+10949),g], cl1[[3]][[i]][i2[1]:(i2[1]+10949),g], cl2[[i]][[3]][i3[1]:(i3[1]+10949),g])
    p2[[g]] <- cbind(ogg[[3]][[i]][(i1[1]):(i1[2]+10949),g], cl1[[3]][[i]][i2[2]:(i2[2]+10949),g], cl2[[i]][[3]][i3[2]:(i3[2]+10949),g])
    p3[[g]] <- cbind(ogg[[3]][[i]][(i1[1]):(i1[3]+10949),g], cl1[[3]][[i]][i2[3]:(i2[3]+10949),g], cl2[[i]][[3]][i3[3]:(i3[3]+10949),g])
    p4[[g]] <- cbind(ogg[[3]][[i]][(i1[1]):(i1[4]+10949),g], cl1[[3]][[i]][i2[4]:(i2[4]+10949),g], cl2[[i]][[3]][i3[4]:(i3[4]+10949),g])
  }
  z0[[i]] <- p0 ; z1[[i]] <- p1 ; z2[[i]] <- p2 ; z3[[i]] <- p3 ; z4[[i]] <- p4
  print('cccccccccccc')
  rm(p0,p1,p2,p3,p4) ; gc()
}

saveRDS(z0,'./NEW/prds/tasmin0.rds',compress = F)
saveRDS(z1,'./NEW/prds/tasmin1.rds',compress = F)
saveRDS(z2,'./NEW/prds/tasmin2.rds',compress = F)
saveRDS(z3,'./NEW/prds/tasmin3.rds',compress = F)
saveRDS(z4,'./NEW/prds/tasmin4.rds',compress = F)

rm(z0,z1,z2,z3,z4) ; gc()

q()
