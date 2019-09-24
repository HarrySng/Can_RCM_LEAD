library(lubridate)
library(viridis)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(reshape2)

plotter <- function(combo, zone) {
  
  lss <- list()
  for(run in 1:50) {
    if (combo == 'warm-dry') {
      lss[[run]] <- overlap_list[[1]][[run]][[zone]]
    } else if (combo == 'warm-wet') {
      lss[[run]] <- overlap_list[[2]][[run]][[zone]]
    } else if (combo == 'cold-dry') {
      lss[[run]] <- overlap_list[[3]][[run]][[zone]]
    } else if (combo == 'cold-wet') {
      lss[[run]] <- overlap_list[[4]][[run]][[zone]]
    }
  }
  yr <- seq(1986,2100,1)
  inds <- c(yr[which(mean_diff[[zone]] >= 0.5)[1]],
            yr[which(mean_diff[[zone]] >= 1.0)[1]],
            yr[which(mean_diff[[zone]] >= 2.0)[1]],
            yr[which(mean_diff[[zone]] >= 3.0)[1]])
  
  plt <- function(k) {
    m <- matrix(NA, nrow = 50, ncol = 5)
    for(run in 1:50) {
      df <- transform(lss[[run]], Freq = pmin(event1, event2)) # Calculate column overlap
      df <- df[,c(1,2,k)]
      colnames(df)[3] <- 'event'
      m[run,1] <- as.numeric(df %>% filter(year >= 1986 & year <= 2015) %>% summarise(sum(event)))
      m[run,2] <- as.numeric(df %>% filter(year >= inds[1] & year <= inds[1]+29) %>% summarise(sum(event)))
      m[run,3] <- as.numeric(df %>% filter(year >= inds[2] & year <= inds[2]+29) %>% summarise(sum(event)))
      m[run,4] <- as.numeric(df %>% filter(year >= inds[3] & year <= inds[3]+29) %>% summarise(sum(event)))
      m[run,5] <- as.numeric(df %>% filter(year >= inds[4] & year <= inds[4]+29) %>% summarise(sum(event)))
    }
    m[,1][m[,1]<=1] <- NA
    m <- na.omit(m)
    m <- data.frame(m)
    colnames(m) <- c('Base','+1.5C','+2.0C','+3.0C', '+4.0C')
    m$id <- k
    return(m)
  }
  
  lss <- lapply(3:5, plt)
  ddf <- do.call(rbind, lss)
  
  ggplot(melt(ddf, 'id')) +
    geom_boxplot(aes(x = variable, y = value)) + theme_bw() +
    xlab('Warming Period') + ylab('Number of Weeks') +
    theme(axis.text.y = element_text(face="bold", size=16),
          axis.text.x = element_text(size = 16, face = 'bold'),
          axis.title.x = element_text(face="bold", size=16),
          axis.title.y = element_text(face="bold", size=16)) + facet_wrap(~id)

}
overlap_list <- readRDS('./rds_obj/clim_overlap_list.rds')
mean_diff <- readRDS('./rds_obj/moving_window_means.rds')
cc <- c('warm-dry','warm-wet', 'cold-dry', 'cold-wet')
plotter(cc[4], 2)
