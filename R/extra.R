######
# here's some nonsense about radii changes on conf intervals and est sd

rm(list = ls())

# tb polygon segment
data(tb_seg)

# secchi data
data(secc_all)

# tb seagrass points
data(sgpts_2010_tb)

##
# subset secc_all to within ten years of sg data
yrs <- strftime(secc_all$Date, '%Y') %>% 
  as.numeric
yrs <- yrs <= 2010 & yrs > 2000
secc_all <- secc_all[yrs, ]

##
# run secchi_doc function with data
rads <- seq(0.01, 0.1, length = 50)

cl <- makeCluster(7)
registerDoParallel(cl)

# process
out_comps <- foreach(rad = seq_along(rads)) %dopar% {
  
  # process, remove segment shapefile, add to output
  tmp <- secc_doc(secc_all, sgpts_2010_tb, tb_seg, radius = rads[rad], '2010', 
    trace = T)
  tmp <- na.omit(tmp[['ave_dat']])
  
  conf_med <- median(tmp$maxd_conf)
  zmax_iqr <- median(tmp$zmax_all)
  
  c(conf_med, zmax_iqr)
  
}

to_plo <- data.frame(do.call('rbind', out_comps))
to_plo$rads <- rads
names(to_plo) <- c('radii', 'conf_med', 'zmax_iqr')

plot(scale(conf_med) ~ radii, data = to_plo, type = 'p')
points(scale(zmax_iqr) ~ radii, data = to_plo, col = 'red')

