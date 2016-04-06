# # comparing uncertainty estimates between old and new methods
# 
# data(ests_out_old)
# 
# datold <- ests_out %>% 
#   select(seg, confint, lat, long) %>% 
#   rename(oldint = confint) %>% 
#   mutate(seg = as.character(seg))
# 
# data(ests_out)
# 
# datnew <- ests_out %>% 
#   select(seg, confint, lat, long) %>% 
#   rename(newint = confint) %>% 
#   mutate(seg = as.character(seg))
# 
# tocomp <- left_join(datold, datnew, by = c('lat', 'long', 'seg'))
# 
# p <- ggplot(tocomp, aes(x = oldint, y = newint)) + 
#   geom_point(size = 3, alpha = 0.7) + 
#   facet_wrap(~seg) + 
#   theme_bw() + 
#   scale_x_continuous('Old', limits = c(0, 3.5)) +
#   scale_y_continuous('New', limits = c(0, 3.5)) + 
#   geom_abline(slope = 1, intercept = 0)
# 
# pdf('C:/Users/mbeck/Desktop/uncert_comp.pdf', height = 8, width = 8, family = 'serif')
# print(p)
# dev.off()

######
# resampling to evaluate prediction interval

library(ggplot2)
library(dplyr)

data(shps)

# segment and seagrass
seg <- shps[['seg_820.shp']] # 820 segment polygon
sgrass <- shps[['sgpts_2006_820.shp']] # 820 segment seagrass depth point

# create sampling grid and test point
set.seed(4321)
est_pts <- grid_est(seg, spacing = 0.02)
test_pt <- est_pts[27, ]

# subset seagrass with buffer around test point
buff_pts <- buff_ext(sgrass, test_pt, buff = 0.06)

# create doc object with samples buffer points
dat_in <- data.frame(buff_pts)
est_vals <- doc_est(dat_in) %>% 
  sens

# get model covar, parameter estimates
mod_est <- attr(est_vals, 'logis_mod')
muvar <- coef(mod_est)
muvar <- muvar[c('xmid', 'scal')]
covar <- vcov(mod_est)
covar <- covar[-1, -1]

# upper/lower 95% prediction interval for the zcmax estimate 
zc <- attr(est_vals, 'z_cmax')
lo <- attr(est_vals, 'lower_est')$z_cmax
hi <- attr(est_vals, 'upper_est')$z_cmax

# simulated data
resamps <- MASS::mvrnorm(n = 1000, mu = abs(muvar), Sigma = covar, empirical = TRUE)
zctest <- resamps[, 1] + 2 * resamps[, 2]
zctest <- data.frame(ests = zctest) 
zctest$outs <- 'within'
zctest$outs[with(zctest, ests < lo | ests > hi)] <- 'outside'

# plot 
p1 <- ggplot(data.frame(zc, lo, hi), aes(x = factor(1), y = zc, ymin = lo, ymax = hi)) + 
  geom_jitter(data = zctest, aes(x = factor(2), y = ests, colour = outs), height = 0, width = 0.2, alpha = 0.6) +
  geom_linerange() + 
  scale_x_discrete(labels = c('Actual', 'Simulated')) + 
  theme(
    axis.title.x = element_blank(),
    legend.position = 'none') + 
  geom_point(size = 2) + 
  ggtitle(paste(sum(zctest$outs == 'within')/nrow(zctest), ' of simulated are within\nthe prediction interval'))
  
pdf('C:/Users/mbeck/Desktop/pi_check.pdf', height  = 5, width = 4, family = 'serif')
print(p1)
dev.off()

