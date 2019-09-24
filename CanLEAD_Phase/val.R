library(Metrics)
rr <- 20
nms1 <- paste('./rds_obj/onerun/Run',seq(1,rr,1),'.rds',sep="") # File names vector
nms2 <- paste('./canlead/ress/Run_',seq(1,rr,1),'.rds',sep="") # File names vector
obsls <- readRDS('./validation/ansp_rgns.rds')

validate <- function(fname) {
  rcmls <- readRDS(fname)
  zone_iter <- function(z) {
    ovardf <- data.frame(pr = rowMeans(obsls[[1]][[z]],na.rm=T), 
                         tmx = rowMeans(obsls[[2]][[z]],na.rm=T), 
                         tmn = rowMeans(obsls[[3]][[z]],na.rm=T))
    rvardf <- rcmls[[z]][1:22280,]
    rvardf$pr <- rvardf$pr*86400 ; rvardf$tmx <- rvardf$tmx-273.15 ; rvardf$tmn <- rvardf$tmn-273.15
    rvardf$date <- ovardf$date <- seq(as.Date('1950-01-01'), as.Date('2010-12-31'),1)
    
    mns <- list(c(12,1,2), c(3,4,5), c(6,7,8), c(9,10,11))
    vbias <- matrix(NA, nrow = 4, ncol = 3)
    for (i in 1:4) {
      df1 <- data.frame(ovardf %>% filter(month(date) %in% mns[[i]]))
      df2 <- data.frame(rvardf %>% filter(month(date) %in% mns[[i]]))
      for (j in 1:3) {
        vbias[i,j] <- bias(df2[,j],df1[,j])
      }
    }
    return(vbias)
  }
  rls <- lapply(1:15, zone_iter)
  rls
}

rls1 <- lapply(nms1, validate)
rls2 <- lapply(nms2, validate)

# rearrange
resls1 <- list()
resls2 <- list()
for (i in 1:15) {
  lss1 <- list()
  lss2 <- list()
  for (j in 1:rr) {
    lss1[[j]] <- rls1[[j]][[i]]
    lss2[[j]] <- rls2[[j]][[i]]
  }
  resls1[[i]] <- lss1
  resls2[[i]] <- lss2
}

# plot
plotter <- function(v) {
  if (v == 1) {
    txt <- 'Mean Bias Error (mm/day)'
  } else {
    txt <- 'Mean Bias Error (°C)'
  }
  zone_iter <- function(z) {
    df1 <- do.call(rbind,resls1[[z]]) ; df2 <- do.call(rbind,resls2[[z]])
    df1 <- df1[,v] ; df2 <- df2[,v]
    df1 <- data.frame(var = df1, ssn = rep(c('Winter','Spring','Summer','Fall'),20))
    df2 <- data.frame(var = df2, ssn = rep(c('Winter','Spring','Summer','Fall'),20))
    win1 <- as.numeric(df1 %>% filter(ssn == 'Winter') %>% summarise(min(var), max(var), mean(var)))
    spr1 <- as.numeric(df1 %>% filter(ssn == 'Spring') %>% summarise(min(var), max(var), mean(var)))
    sum1 <- as.numeric(df1 %>% filter(ssn == 'Summer') %>% summarise(min(var), max(var), mean(var)))
    fal1 <- as.numeric(df1 %>% filter(ssn == 'Fall') %>% summarise(min(var), max(var), mean(var)))
    win2 <- as.numeric(df2 %>% filter(ssn == 'Winter') %>% summarise(min(var), max(var), mean(var)))
    spr2 <- as.numeric(df2 %>% filter(ssn == 'Spring') %>% summarise(min(var), max(var), mean(var)))
    sum2 <- as.numeric(df2 %>% filter(ssn == 'Summer') %>% summarise(min(var), max(var), mean(var)))
    fal2 <- as.numeric(df2 %>% filter(ssn == 'Fall') %>% summarise(min(var), max(var), mean(var)))
    dfp1 <- data.frame(t(rbind(win1,spr1,sum1,fal1))) ; dfp1$zone <- z ; dfp1$data <- 'CanRCM4'
    dfp2 <- data.frame(t(rbind(win2,spr2,sum2,fal2))) ; dfp2$zone <- z ; dfp2$data <- 'CanLEAD'
    colnames(dfp1) <- colnames(dfp2) <- c('win','spr','sum','fal','zone','data')
    ddf <- rbind(dfp1,dfp2)
    ddf
  }
  rls <- lapply(1:15, zone_iter)
  rls <- data.frame(do.call(rbind,rls))
  rls$id <- rep(c('min','max','mean'),30) ; rls$id <- as.factor(rls$id)
  colnames(rls)[1:5] <- c('Winter','Spring','Summer','Fall','Ecozone')
  
  dfp <- melt(rls,c('Ecozone','id','data'))
  dfp$data <- as.factor(dfp$data)
  
  #g <- ggplot(dfp) + 
  #  geom_point(aes(x = Ecozone, y = value, color = data, shape = id, size = id), show.legend = F) +
  #  scale_color_manual(values = c('grey25','black')) + scale_shape_manual(values = c(95, 19, 95)) +
  #  scale_size_manual(values = c(7,4,7)) +
  #  theme_bw() + facet_wrap(~variable) +
  #  xlab('Zone') + ylab(txt) +
  #  theme(axis.text.y = element_text(face="bold", size=16),
  #        axis.text.x = element_text(size = 16, face = 'bold'),
  #        axis.title.x = element_text(face="bold", size=16),
  #        axis.title.y = element_text(face="bold", size=16),
  #        strip.text.x = element_text(face = 'bold', size = 16)) +
  #  scale_x_continuous(breaks = c(1,3,6,9,12,15)) + geom_hline(yintercept = 0, linetype = 'dashed')
  #tiff('./validation/val_tmin.tiff', units="in", width=8, height=6, res=200)
  #print(g)
  #dev.off()
  
  ############# BARPLOT
  
  dd <- dfp %>% filter(id == 'mean')
  dd$id <- NULL ; colnames(dd)[4] <- 'mean'
  dd2 <- dfp %>% filter(id == 'min')
  dd$min <- dd2$value
  dd2 <- dfp %>% filter(id == 'max')
  dd$max <- dd2$value
  
  g <- ggplot(dd, aes(x=Ecozone, ymin=min, ymax=max, fill=data)) +
    geom_bar(position=position_dodge(), aes(y=mean), stat="identity", show.legend = T, width = 0.7) +
    geom_errorbar(position=position_dodge(width=0.9), colour="black", width = 0.5) +
    scale_fill_manual(values = c('#E64B35B2','#00A087B2')) + theme_bw() + facet_wrap(~variable, nrow = 4) +
    xlab('Zone') + ylab(txt) +
    theme(axis.text.y = element_text(face="bold", size=16),
          axis.text.x = element_text(size = 16, face = 'bold'),
          axis.title.x = element_text(face="bold", size=16),
          axis.title.y = element_text(face="bold", size=16),
          strip.text.x = element_text(face = 'bold', size = 16)) +
    scale_x_continuous(breaks = seq(1,15,1)) + geom_hline(yintercept = 0, linetype = 'dashed')
  tiff('./validation/val_tmin.tiff', units="in", width=10, height=6, res=200)
  print(g)
  dev.off()
}