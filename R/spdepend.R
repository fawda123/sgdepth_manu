load('data/shps.RData')

# iters to test
spcs <- seq(0.01, 0.05, length = 20)
rads <- seq(0.01, 0.1, length = 20)
segs <- c('303', '820', '902', '1502')
seeds <- sample(seq(1, 5000), 20)
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
  
  # get grid and estimates using same starting location each grid
  set.seed(seed_val)
  smps <- grid_est(seg, spacing = spc_val)
  ests <- doc_est_grd(smps, sgdep, radius = rad_val)

  # get distance matrices, location then responses
  spdep <- try({
    
    ests_dists <- dist(coordinates(ests))
    ests_maxd <- dist(data.frame(ests)$doc_max)

    mantel.rtest(ests_dists, ests_maxd)$obs
    
  }, silent = T)
  
  if('try-error' %in% class(spdep)) spdep <- NA
  spdep
  
}

res <- data.frame(iter_grd, corrs = unlist(out))
save(res, file = 'C:/Users/mbeck/Desktop/res.RData')

# get means, sds by seeds
res_agg <- ddply(res, 
  .variables = c('spcs', 'rads', 'segs'), 
  .fun = function(x) mean(x$corrs, na.rm = T)
)

ggplot(res_agg, aes(x = rads, y = V1, group = spcs, colour = spcs)) + 
  geom_line() + 
  geom_point() +
  theme_bw() + 
  facet_wrap(~segs, scales = 'free_y')
