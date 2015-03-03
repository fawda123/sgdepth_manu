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

######
# secchi boxplots for stations in each segment
######
#

data(secc_all)
data(irl_seg)

secc_dat <- secc_all
seg_shp <- irl_seg

# clip secchi by seg
# clip secchi data by segments
sel <- !is.na(secc_dat %over% seg_shp)[, 1]
secc <- secc_dat[sel, ]

# select only years within last ten of seagrass survey
yrs <- strftime(secc$Date, '%Y') %>% 
  as.numeric
yrs <- yrs <= 2010 & yrs > 2000
secc <- secc[yrs, ]

# get unique locations of secchi data
uni_secc <- data.frame(secc)[, c('Station_ID', 'Longitude', 'Latitude')]
uni_secc <- unique(uni_secc)
uni_secc <- SpatialPointsDataFrame(
  coords = uni_secc[, c('Longitude', 'Latitude')], 
  data = uni_secc[, 'Station_ID', drop = F]
  )

secc$seg <- NA
for(seg in as.character(irl_seg$Segment)){
  
  to_sel <- irl_seg[irl_seg$Segment %in% seg, ]
  sel <- !is.na(secc %over% to_sel)[, 1]
  secc[sel, 'seg'] <- as.character(seg)
  
}

# merge locations
secc <- dplyr::left_join(data.frame(secc), data.frame(uni_secc), 
  by = 'Station_ID') %>% 
  mutate(SD = as.numeric(SD))

# remove stations with only a few obs
rems <- table(secc$Station_ID)
rems <- names(rems)[rems < 10]
secc <- secc[!secc$Station_ID %in% rems, ]

ggplot(secc, aes(x = Station_ID, y = SD)) + 
  geom_boxplot() +
  facet_wrap(~ seg, scales = 'free_x')


