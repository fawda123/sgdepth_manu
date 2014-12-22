library(ape)

load('data/shps.RData')

source('R/funcs.R')

# values to iterate
spc_val <- 0.02
rad_val <- 0.005
seg_val <- '1502'
seed_val <- 1234

sgpts_nm <- grep(paste0('^sgpts.*', seg_val, '\\.shp'), names(shps))
seg_nm <- paste0('seg_', seg_val, '.shp') 
sgdep <- shps[[names(shps)[sgpts_nm]]]
seg <- shps[[seg_nm]]
  
# get grid and estimates using same starting location each grid
set.seed(seed_val)
smps <- grid_est(seg, spacing = spc_val)
# point from random points for buffer
rand <- 10 #sample(1:length(smps), 1)
test_pt <- smps[rand, ]
  
# get bathym points around test_pt
buff_pts <- buff_ext(sgdep, test_pt, buff = rad_val)

# get plot
# plot_doc_est(buff_pts)

# sensitivity

depth_var <- 'Depth'
ests <- doc_est(buff_pts)


newdat <- data.frame(Depth = ests$pred[, depth_var])
xvar <- ests$data[, depth_var, drop = F]
tmp <- predictNLS(ests$logis_mod, newdata = data.frame(newdat, error = 0), nsim = 10000)
unc <- data.frame(Depth = ests$pred[, depth_var], tmp)

plot(X97.5. ~ Depth, data = unc, type = 'l', col = 'tomato')
lines(fit ~ Depth, data = unc, col = 'black')
lines(X2.5. ~ Depth, data = unc, col = 'tomato')




