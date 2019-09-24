library(lubridate)
library(tidyverse)
library(reshape2)

dts <- readRDS('./rds_obj/RCM_Dates.rds')
dts <- dts[which(dts=='1986-01-01'):which(dts=='2015-12-31')]

prls <- readRDS('./rds_obj/pr_savg_periods_anomaly.rds')
tmxls <- readRDS('./rds_obj/tmx_savg_periods_anomaly.rds')
tmnls <- readRDS('./rds_obj/tmn_savg_periods_anomaly.rds')

corr <- function(pr,tmx,tmn,ii) {
  cmat <- matrix(nrow = 200, ncol = 5)
  for (i in 1:5) {
    p <- pr[[i]] 
    t <- (tmx[[i]]+tmn[[i]])/2
    p <- data.frame(p)
    t <- data.frame(t)
    p$date <- t$date <- dts
    
    ssn_maker <- function(d,mns,f) {
      d <- data.frame(d %>% filter(month(date) %in% mns) %>% group_by(year(date)) %>% mutate(date = NULL) %>% summarise_all(eval(parse(text=f))))
      d <- as.matrix(d[,2:51])
      d
    }
    
    pwin <- ssn_maker(p,c(12,1,2),'sum') ; pspr <- ssn_maker(p,c(3,4,5),'sum') ; psum <- ssn_maker(p,c(6,7,8),'sum') ; pfal <- ssn_maker(p,c(9,10,11),'sum')
    twin <- ssn_maker(t,c(12,1,2),'mean') ; tspr <- ssn_maker(t,c(3,4,5),'mean') ; tsum <- ssn_maker(t,c(6,7,8),'mean') ; tfal <- ssn_maker(t,c(9,10,11),'mean')
    
    cmat[1:50,i] <- sapply(1:50, function(i) cor.fk(pwin[,i], twin[,i]))
    cmat[51:100,i] <- sapply(1:50, function(i) cor.fk(pspr[,i], tspr[,i]))
    cmat[101:150,i] <- sapply(1:50, function(i) cor.fk(psum[,i], tsum[,i]))
    cmat[151:200,i] <- sapply(1:50, function(i) cor.fk(pfal[,i], tfal[,i]))
  }
  
  cmat <- data.frame(cmat)
  colnames(cmat) <- c('Base','+1.5C','+2C','+3C','+4C')
  cmat$ssn <- rep(c('Winter','Spring','Summer','Fall'),each=50)
  ggplot(melt(cmat)) +
    geom_boxplot(aes(x = variable, y = value)) + 
    theme_bw() + xlab('Period') + ylab("kendall's Rank Coefficient") +
    theme(axis.text.y = element_text(face="bold", size=14),
          axis.text.x = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(face="bold", size=14),
          axis.title.y = element_text(face="bold", size=14),
          strip.text.x = element_text(face = 'bold', size = 14)) + facet_wrap(~ssn)
}

#lapply(1:15, function(i) corr(prls[[i]],tmxls[[i]],tmnls[[i]],i))

k <- 8
corr(prls[[k]],tmxls[[k]],tmnls[[k]],k)
