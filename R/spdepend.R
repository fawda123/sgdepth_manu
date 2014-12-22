library(ape)

load('data/shps.RData')

source('R/funcs.R')

# iters to test
spcs <- seq(5, 40, length = 35) # points on long side
rads <- 0.05# seq(0.01, 0.25, length = 20) # percent of dec degrees on long side
segs <- c('1502','303', '820', '902', '1502')
seeds <- sample(seq(1, 5000), 100)
iter_grd <- expand.grid(spcs, rads, segs, seeds)
names(iter_grd) <- c('spcs', 'rads', 'segs', 'seeds')

# setup parallel
cl <- makeCluster(7)
registerDoParallel(cl)
strt <- Sys.time()
out <- foreach(iter = 1:nrow(iter_grd)) %dopar% {

  # progress
  sink('log.txt')
  cat(iter, 'of', nrow(iter_grd), '\n')
  print(Sys.time() - strt)
  sink()
  
  # values to iterate
  spc_val <- iter_grd[iter, 'spcs']
  rad_val <- iter_grd[iter, 'rads']
  seg_val <- iter_grd[iter, 'segs']
  seed_val <- iter_grd[iter, 'seeds']
  
  sgpts_nm <- grep(paste0('^sgpts.*', seg_val, '\\.shp'), names(shps))
  seg_nm <- paste0('seg_', seg_val, '.shp') 
  sgdep <- shps[[names(shps)[sgpts_nm]]]
  seg <- shps[[seg_nm]]
  
  ## convert spc ests and rads to dec. degree
  
  # get spatial extent, find longest side
  extent <- bbox(seg)
  which_side <- which.max(abs(extent[, 2] - extent[, 1]))
  extent <- extent[which_side, ]
  
  # reassign spc_val based on number of pts that can fit on long side
  spc_val <- diff(extent)/(1 +spc_val)
  
  # rad_val is a percent, convert to dec. degree based on long side
  rad_val <- diff(extent) * rad_val
  
  # get grid and estimates using same starting location each grid
  set.seed(seed_val)
  smps <- grid_est(seg, spacing = spc_val)
  ests <- doc_est_grd(smps, sgdep, radius = rad_val)
  names(ests) <- c('sg_max', 'doc_med', 'doc_max')
  
  # remove really high values if there are results
  if(sum(is.na(ests$doc_max)) != nrow(ests)){
    filt_val <- quantile(ests$doc_max, 0.95, na.rm = T)
    ests <- ests[ests$doc_max <= filt_val, ]
  }
  
  # get distance matrix, then Moran I
  spdep <- try({
    
    dists <- coordinates(ests)
    dists <- as.matrix(dist(cbind(dists[, 'Var1'], dists[ ,'Var2'])))

    dists_inv <- 1/dists
    diag(dists_inv) <- 0
    
    ape::Moran.I(ests$doc_max, dists_inv)$observed
    
  }, silent = T)
  
  if('try-error' %in% class(spdep)) spdep <- NA
  
  # get mean of ests
  var_est <- var(ests$doc_max, na.rm = T)
  mean_est <- mean(ests$doc_max, na.rm = T)
  
  c(spdep, var_est, mean_est)
 
}

res <- data.frame(iter_grd, corrs = do.call('rbind', out))
names(res)[names(res) %in% c('corrs.1', 'corrs.2')] <- c('corr', 'mean')
save(res, file = 'C:/Users/mbeck/Desktop/res.RData')

# get means, sds by seeds
res_agg <- ddply(res, 
  .variables = c('spcs', 'rads', 'segs'), 
  .fun = function(x){
    mean_corr <- mean(x$corr, na.rm = T)
    var_mean_corr <- var(x$corr, na.rm = T)
    var_mean <- var(x$mean, na.rm =  T)
    c(mean_corr, var_mean_corr, var_mean)
  }
)
names(res_agg)[names(res_agg) %in% c('V1', 'V2', 'V3')] <- c('corr', 'var_corr', 'var_mean')

# mean spatial corr across all seeds
ggplot(res_agg, aes(x = spcs, y = corr)) + 
  geom_line() + 
  geom_point() +
  geom_line(aes(x = spcs, y = corr + var_corr), colour = 'lightblue') +
  geom_line(aes(x = spcs, y = corr - var_corr), colour = 'lightblue') + 
  theme_bw() + 
  facet_wrap(~segs, scales = 'free_y')

# variance of the mean estimates across all seeds
ggplot(res_agg, aes(x = spcs, y = var_mean)) + 
  geom_line() + 
  geom_point() +
  theme_bw() + 
  facet_wrap(~segs, scales = 'free_y')
