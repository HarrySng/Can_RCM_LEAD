library(Metrics)
library(lubridate)
library(tidyverse)
library(doParallel)

mm <- c('bias','mae','rmse')
lbl <- c('Mean Bias Error (MBE)', 'Mean Absolute Error(MAE)', 'Root mean Squared Error (RMSE)')
ssn <- c('winter','spring','summer','fall')

val_wrapper <- function(v) {
  rls <- readRDS(paste('./NEW/',v,'_valres.rds',sep=""))
  
  for (s in 1:4) {
    ddf <- rls[[s]]
    plotter <- function(met) {
      if(met==1) {k<-1:3
      } else if (met==2) {k<-4:6
      } else if (met==3) {k<-7:9}
      
      d <- ddf[,c(10,11,k)]
      d <- na.omit(d)
      colnames(d) <- c('lon','lat','CanRCM4','CanLEAD1','CanLEAD2')
      
      dfp <- melt(d, c('lon','lat'))
      g <- ggplot(dfp) +
        geom_point(aes(x = lon, y = lat, color = value), shape = 15) +
        scale_color_distiller(palette = 'Spectral') +
        theme_bw() + facet_wrap(~variable, nrow = 1, ncol = 3) + ggtitle(lbl[met])
      tiff(paste('./NEW/plots/',v,'_',mm[met],'_',ssn[s],'.tiff',sep=""), units="in", width=9, height=3, res=200)
      print(g)
      dev.off()
    }
    plotter(1)
    plotter(2)
    plotter(3)
  }
}

lapply(c('pr','tasmax','tasmin'), val_wrapper)


box_wrapper <- function(v) {
  rls <- readRDS(paste('./NEW/',v,'_valres.rds',sep=""))
  
  for (s in 1:4) {
    ddf <- rls[[s]]
    plotter <- function(met) {
      if(met==1) {k<-1:3
      } else if (met==2) {k<-4:6
      } else if (met==3) {k<-7:9}
      
      d <- ddf[,c(10,11,k)]
      d <- na.omit(d)
      colnames(d) <- c('lon','lat','CanRCM4','CanLEAD1','CanLEAD2')
      
      dfp <- melt(d, c('lon','lat'))
      
      g <- ggplot(dfp) +
        geom_boxplot(aes(x = variable, y = value), outlier.size = 2, shape = 1) +
        theme_bw() + xlab('Dataset') + ylab(paste(lbl[met])) +
        theme(axis.text.x = element_text(face = 'bold', size = 14),
              axis.text.y = element_text(face = 'bold', size = 14),
              axis.title.x = element_text(face = 'bold', size = 14),
              axis.title.y = element_text(face = 'bold', size = 14))
      tiff(paste('./NEW/plots/Box_',v,'_',mm[met],'_',ssn[s],'.tiff',sep=""), units="in", width=6, height=4, res=200)
      print(g)
      dev.off()
    }
    plotter(1)
    plotter(2)
    plotter(3)
  }
}

lapply(c('pr','tasmax','tasmin'), box_wrapper)
