library(tidyverse)
library(reshape2)
library(stringr)

gd <- readRDS('./canlead_zone_ind.rds')
gd <- do.call(rbind,gd)
source('clim_func_names.R')
func_names <- func_names[c(3,4,5,6,12,13,7,16,19,21,22,24,25,26)]
func_names <- word(func_names,-1,sep="\\.")

plotter <- function(p) {
  fls <- paste('./NEW/clims/zone_',seq(1,15,1),'_p_',p,'.rds',sep="")
  dls <- lapply(fls, readRDS)
  
  cc <- function(c) {
    print('Looping')
    rlsh <- rlsf <- list()
    for (r in 1:3) {
      hsls <- list()
      ftls <- list()
      for (z in 1:15) {
        gls <- list()
        for (g in 1:length(dls[[z]])) {
          gls[[g]] <- dls[[z]][[g]][[r]][[c]]
        }
        m <- do.call(rbind,gls) 
        hsls[[z]] <- rowMeans(m[,(1:(dim(m)[2]/2))])
        ftls[[z]] <- rowMeans(m[,((dim(m)[2]/2)+1):(dim(m)[2])])
      }
      rlsh[[r]] <- unlist(hsls)
      rlsf[[r]] <- unlist(ftls)
    }
    
    mm <- cbind(do.call(cbind,rlsh),do.call(cbind,rlsf))
    saveRDS(mm,paste('./NEW/clims/plot_ready/P_',p,'_',func_names[c],'.rds',sep=""),compress = F)
  }
  lapply(1:14, cc)
}

for (i in 1:4) {
  plotter(i)
}

q()
