library(rstan)
library(tidyverse)

stan_model <- readRDS('./HB/fit_Summer.rds')
pls <- rstan::extract(stan_model, permuted=T)
#######################################################################################################

# Section to see summary statistics
summary(pls$t_beta) # Trend of Tmp ~ Time before year 2000
summary(pls$t_gma) # Trend of Tmp ~ Time after year 2000

summary(pls$p_beta) # Trend of Pr ~ Time before year 2000
summary(pls$p_gma) # Trend of Pr ~ Time after year 2000
#######################################################################################################

# Section to analyse biases of each model
plot_bias_bars <- function(..) {
  t_bias <- c()
  p_bias <- c()
  for (i in 1:dim(pls$d_t)[2]) {
    t_bias[i] <- mean(pls$d_t[,i])
    p_bias[i] <- mean(pls$d_p[,i])
  }
  df <- data.frame(x = 1:150, tb = t_bias, pb = p_bias, mdl = rep(c('CanRCM4','CanLEAD1','CanLEAD2'),each=50))
  df1 <- melt(df,c('x','mdl','pb'))
  df2 <- melt(df,c('x','mdl','tb'))
  df1$mdl = factor(df1$mdl,levels(df1$mdl)[c(3,1,2)]) # sort levels
  df2$mdl = factor(df2$mdl,levels(df2$mdl)[c(3,1,2)]) # sort levels
  
  ggplot(df1) +
    geom_bar(aes(x = x, y = value, fill = mdl), stat = 'identity') +
    scale_fill_lancet() + theme_bw()
  
  ggplot(df2) +
    geom_bar(aes(x = x, y = value, fill = mdl), stat = 'identity') +
    scale_fill_lancet() + theme_bw()
}
#######################################################################################################

# Section to look at correlations
plot_cors <- function(..) {
  obs_cor <- pls$beta_xo
  mdl_cor <- pls$beta_x
}
#######################################################################################################


