library(tidyverse) ; library(viridis)

fls <- paste('./rds_obj/clim_spatial/Clim_result_final_',seq(1,15,1),'.rds',sep="")
grd <- readRDS('./rds_obj/CanRCM_Grids_By_Zone.rds')
grd_new <- readRDS('./rds_obj/CanRCM_Grids_By_Zone_NEW.rds')
gg <- left_join(grd, grd_new, c('lon','lat'))
naind <- which(is.na(gg$Zone))
gg <- gg[-naind,]
clim_list <- lapply(fls, readRDS)
clim <- c('su','wsdi','csdi','sdii','cdd','cwd','r99')
dfp <- readRDS('./rds_obj/final_pop.rds')

plotter <- function(c) {
  rowid <- c(1,31,61,91,121)
  prdls <- list()
  for (period in rowid) {
    runls <- list()
    for (run in 1:50) {
      zonels <- list()
      for (zone in 1:15) {
        gridls <- list()
        for (grid in 1:length(clim_list[[zone]][[run]])) {
          gridls[[grid]] <- clim_list[[zone]][[run]][[grid]][period:(period+29),c]
        }
        zonels[[zone]] <- round(rowMeans(do.call(rbind, gridls)))
      }
      runls[[run]] <- unlist(zonels)
    }
    prdls[[length(prdls)+1]] <- do.call(rbind,runls)
  }
  
  m <- matrix(NA, nrow = 5133, ncol = 5)
  for (i in 1:5) {
    m[,i] <- prdls[[i]][1,]
  }
  m <- m[-naind,]
  m <- data.frame(m) 
  colnames(m) <- c('Base','+1.5C','+2C','+3C','+4C')
  m$lon <- gg$lon ; m$lat <- gg$lat ; m$zone <- gg$zone
  
  for (z in 1:15) {
    d <- data.frame(m %>% filter(zone == z))
    expo <- c()
    for (j in 1:5) {
      expo[j] <- dfp$pop[z] * sum(d[,j])
    }
  }
  
  
  
  
  
  g <- ggplot(data = df) + geom_point(aes(x = lon, y = lat, color = value), size = 1.2, shape = 15) + 
    geom_point(aes(x = lon, y = lat, alpha = id), color = 'black', size = 0.1, shape = 8) + 
    scale_alpha_manual(values = c(0,0.5)) +
    scale_color_distiller(palette = 'Spectral') + theme_bw() + facet_grid(cols = vars(df$variable)) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text.x = element_text(face = "bold", size = 16),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  tiff(paste(clim[c],'.tiff',sep=""), units="in", width=10, height=4, res=200)
  print(g)
  dev.off()
  
}
