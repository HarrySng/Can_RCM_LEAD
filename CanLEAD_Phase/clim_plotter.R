library(tidyverse)
library(reshape2)
library(stringr)

gd <- readRDS('./canlead_zone_ind.rds')
gd <- do.call(rbind,gd)
source('./Scripts/clim_func_names.R')
func_names <- func_names[c(3,4,5,6,12,13,7,16,19,21,22,24,25,26)]
func_names <- word(func_names,-1,sep="\\.")
prd <- c('Base','p1','p2','p3','p4')

plotter <- function(c) {
  fls <- list.files(path = './NEW/clims/', pattern = paste(func_names[c]), full.names = T)
  dls <- lapply(fls, readRDS)
  dfb <- data.frame(dls[[1]])
  colnames(dfb) <- c('NRCANmet','CanRCM4','CanLEAD1','CanLEAD2')
  dfb$lon <- gd$lon ; dfb$lat <- gd$lat
  dfb <- na.omit(dfb)
  dfb <- melt(dfb, c('lon','lat'))
  i <- 1
  g <- ggplot(dfb) +
    geom_point(aes(x = lon, y = lat, color = value), shape = 15) +
    scale_color_distiller(palette = 'Spectral') +
    theme_bw() + facet_wrap(~variable, nrow = 1, ncol = 4)
  tiff(paste('./NEW/plots/clim_plots/',prd[i],'_',func_names[c],'.tiff',sep=""), units="in", width=9, height=3, res=200)
  print(g)
  dev.off()
  
  for (i in 2:5) {
    dfh <- data.frame(dls[[i]][,1:3])
    dff <- data.frame(dls[[i]][,4:6])
    colnames(dfh) <- colnames(dff) <- c('CanRCM4','CanLEAD1','CanLEAD2')
    dfh$lon <- dff$lon <- gd$lon ; dff$lat <- dfh$lat <- gd$lat
    
  }
  
}
lapply(1:14,plotter)
