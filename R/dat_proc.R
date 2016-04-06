######
# summary of wbid characteristics

# create shapefile object of seagrass coverages
id <- c('sg_2007_303.shp', 'sg_2006_820.shp', 'sg_2010_902.shp', 'sg_2009_1502.shp')
sg_shps <- list()
for(i in id){
  
  path <- paste0('M:/GIS/seagrass/', i)
  sg <- readShapeSpatial(path)
  sg_shps[[i]] <- sg
  
}
save(sg_shps, file = 'data/sg_shps.RData')

# bathymetry data
id <- c('dep_303.shp', 'dep_820.shp', 'dep_902.shp', 'dep_1502.shp')
dep_shps <- list()
for(i in id){
  
  cat(i, '\n')
  path <- paste0('M:/GIS/seagrass/', i)
  dep <- readShapeSpatial(path)
  dep_shps[[i]] <- dep
  
}
save(dep_shps, file = 'data/dep_shps.RData')

load(file = 'data/shps.RData')
load(file = 'data/sg_shps.RData')
load(file = 'data/secc_all.RData')
load(file = 'data/dep_shps.RData')

nm <- c('W. Choctawhatchee Bay', 'Big Bend', 'Old Tampa Bay', 'Upper Indian River Lagoon')

# lat, long
lat <- sapply(
  c('seg_303.shp', 'seg_820.shp', 'seg_902.shp', 'seg_1502.shp'), 
  function(x) data.frame(rgeos::gCentroid(shps[[x]]))[, 2]
)
lon <- sapply(
  c('seg_303.shp', 'seg_820.shp', 'seg_902.shp', 'seg_1502.shp'), 
  function(x) data.frame(rgeos::gCentroid(shps[[x]]))[, 1]
)

# segment, seagrass, prop areas
areas <- sapply(
  c('303', '820', '902', '1502'), 
  function(x){
    
    # data and projections
    seg <- shps[[paste0('seg_', x, '.shp')]]
    sg <- sg_shps[[grep(paste0(x, '\\.shp'), names(sg_shps))]]
    proj4string(seg) <- CRS("+proj=longlat +datum=NAD83")
    proj4string(sg) <- CRS("+proj=longlat +datum=NAD83")
    
    # proj for transformation
    trans_proj <- CRS("+proj=utm +zone=17 +datum=NAD83")
    if(x == '303') trans_proj <- CRS("+proj=utm +zone=16 +datum=NAD83")
    seg <- spTransform(seg, trans_proj)  
    sg <- spTransform(sg, trans_proj)
    
    # clip, get area in sq km
    clip_sg <- gIntersection(sg, seg, byid = TRUE) #, drop_lower_td = TRUE)
    seg_est <- gArea(seg)/1e6
    sg_est <- gArea(clip_sg)/1e6
    
    # list output
    c(seg_est, sg_est)
      
  })

# segment depths
deps <- sapply(
  c('303', '820', '902', '1502'), 
  function(x){
    
    tmp <- dep_shps[[paste0('dep_', x, '.shp')]]
    nm <- c('depth', 'GRID_CODE', 'Depth')
    nm <- names(tmp)[names(tmp) %in% nm]
    tmp <- -1 * data.frame(tmp)[, nm, drop = T]
    
    tmp <- tmp[tmp <= quantile(tmp, 0.99)]
    mn_val <- mean(tmp, na.rm = T)
    max_val <- max(tmp, na.rm = T)
  
    c(mn_val, max_val)
    
  })

# segment secchi
seccs <- sapply(
  c('303', '820', '902', '1502'), 
  function(x){
    
    # subset secchi
    seg <- shps[[paste0('seg_', x, '.shp')]]
    secc <- !is.na(secc_all %over% seg)[, 1]
    secc <- secc_all[secc, ]
    secc$yr <- as.numeric(strftime(secc$Date, '%Y'))
    
    # subset by most recent ten years
    yr_vals <- data.frame(
      seg = c('303', '820', '902', '1502'), 
      yrs = c(2007, 2006, 2010, 2009)
      )
    yr <- yr_vals[yr_vals$seg == x, 'yrs']
    secc <- secc[secc$yr > yr - 10 & secc$yr <= yr, ]
    
    # mean secchi and n
    ave_secc <- mean(as.numeric(secc$SD), na.rm = T)
    se_secc <- sd(as.numeric(secc$SD), na.rm = T)/sqrt(length(na.omit(secc$SD)))
      
    c(ave_secc, se_secc)
    
  })
# fill in 820 manually, data are from elsewhere
seccs[, 2] <- c(1.34, 0.19)

# combine data
out <- data.frame(rbind(lat, lon, areas, deps, seccs), stringsAsFactors = F)
names(out) <- nm
row.names(out) <- c('Latitude', 'Longitude', 'Surface area', 'Seagrass area', 'Depth (mean)', 'Depth (max)', 'Secchi (mean)', 'Secchi (se)')
  
seg_summ <- out
save(seg_summ, file = 'data/seg_summ.RData')

######
# get doc ests for whole seg

load('data/shps.RData')

segs <- c('303', '820', '902', '1502')

ests_seg <- vector('list', length = length(segs))
names(ests_seg) <- segs

for(seg in segs){
    
  seg_shp <- shps[[paste0('seg_', gsub('^.*_', '', seg), '.shp')]]
  sgpts_shp <- shps[[grep(paste0('^sgpts.*', seg, '.shp$'), names(shps))]]
  
  rad <- 0.25 # sufficient for each seg
  test_loc <- rgeos::gCentroid(seg_shp)
  ests <- doc_est_grd(test_loc, sgpts_shp, radius = rad)

  ests_seg[[seg]] <- data.frame(ests, seg = seg)
  
}

ests_seg <- do.call('rbind', ests_seg)
names(ests_seg)[names(ests_seg) %in% c('x', 'y')] <- c('long', 'lat')

save(ests_seg, file = 'data/ests_seg.RData')

######
# get estimates for grids

load('data/shps.RData')
segs <- c('303', '820', '902', '1502')

ests_out <- vector('list', length = length(segs))
names(ests_out) <- segs

for(seg in segs){
    
  seg_shp <- shps[[paste0('seg_', gsub('^.*_', '', seg), '.shp')]]
  sgpts_shp <- shps[[grep(paste0('^sgpts.*', seg, '.shp$'), names(shps))]]
  
  grid_spc <- 0.01
  rad <- 0.02
  grid_seed <- 4321
  set.seed(grid_seed)
  pts <- grid_est(seg_shp, spacing = grid_spc) 
  ests <- doc_est_grd(pts, sgpts_shp, radius = rad, out_sens = T, remzero = F)

  ests_out[[seg]] <- data.frame(ests, seg = seg)
  
}

ests_out <- do.call('rbind', ests_out)
names(ests_out)[names(ests_out) %in% c('Var1', 'Var2')] <- c('long', 'lat')

# get ci size, these are all the same for each measure
# get lower bound of z_cmax for eval of those greater than zero
ests_out$confint <- with(ests_out, h_z_cmin - l_z_cmin)
ests_out$lower_max <- ests_out$l_z_cmax
ests_out <- ests_out[, !grepl('^h_|^l_', names(ests_out))]

# remove outliers from ests based on 99%
ests <- ests_out[, c('z_cmin', 'z_cmed', 'z_cmax')]
filt_val <- quantile(unlist(c(ests)), 0.99)
ests[ests > filt_val] <- NA
ests_out[, c('z_cmin', 'z_cmed', 'z_cmax')] <- ests
ests_out <- na.omit(ests_out)

ests_out_nonsigs <- ests_out
ests_out <- ests_out[ests_out$lower_max >= 0, ]

# remove outliers for bb and tb
ests_out <- ests_out[!row.names(ests_out) %in% c('820.159', '1502.82'), ]

save(ests_out_nonsigs, file = 'data/ests_out_nonsigs.RData')
save(ests_out, file = 'data/ests_out.RData')

######
# Choctawhatchee light

source('R/funcs.r')

# load data
data(choc_sats_crc)
data(choc_seg)
data(sgpts_2007_choc)
ave_rast <- choc_sats_crc[['ave_rast']]

# create sampling grid, 0.01 dec degree spacing
choc_grd <- grid_est(choc_seg, 0.005)

# sample the satellit clarity raster
samp_vals <- raster::extract(ave_rast, choc_grd, df = TRUE)[, -1]
samp_vals <- na.omit(samp_vals)
names(samp_vals) <- c('KD', 'Longitude', 'Latitude')
coordinates(samp_vals) <- c('Longitude', 'Latitude')

# process for seagrass depth limits and light requirements
proc <- kd_doc(samp_vals, sgpts_2007_choc, choc_seg, radius = 0.04, trace = T)
dat <- na.omit(proc)

choc_light <- dat
save(choc_light, file = 'data/choc_light.RData')

######
# TB light requirements

source('R/funcs.r')

# load data
data(tb_sats)
data(tb_seg)
data(sgpts_2010_tb)
ave_rast <- tb_sats[['ave_rast']]

# create sampling grid, 0.01 dec degree spacing
tb_grd <- grid_est(tb_seg, 0.01)

# sample the satellit clarity raster
samp_vals <- raster::extract(ave_rast, tb_grd, sp = T) %>% 
  data.frame  
names(samp_vals) <- c('clarity', 'Longitude', 'Latitude')
coordinates(samp_vals) <- c('Longitude', 'Latitude')

# process for seagrass depth limits and light requirements
proc <- secc_doc(samp_vals, sgpts_2010_tb, tb_seg, seg_pts_yr = 2010, radius = 0.1, trace = T)
dat <- na.omit(proc)

tb_light <- dat
save(tb_light, file = 'data/tb_light.RData')

######
# irl light requirements

source('R/funcs.r')

# irl polygon segment
data(irl_seg)

# irl secchi data
data(secc_all)

# irl seagrass points
data(sgpts_2009_irl)

# select only years within last ten of seagrass survey
yrs <- strftime(secc_all$Date, '%Y') %>% 
  as.numeric
yrs <- yrs <= 2009 & yrs > 1999
secc_all <- secc_all[yrs, ]

# remove stations with less than five observations
rems <- table(secc_all$Station_ID)
rems <- names(rems)[rems < 5]
secc_all <- secc_all[!secc_all$Station_ID %in% rems, ]

# process, get ave secchi data results
proc <- secc_doc(secc_all, sgpts_2009_irl, irl_seg, radius = 0.15, seg_pts_yr = '2009', trace = T)
dat <- na.omit(proc)

#remote bad secchi values and low conf
dat <- dat[dat$light > 4 & dat$maxd_conf < 1 , ] 

irl_light <- dat
save(irl_light, file = 'data/irl_light.RData')