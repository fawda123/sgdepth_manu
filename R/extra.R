######
# eval of seagrass doc with secchi depth

######
# for individual wbids

# lots of shapefile data
data(shps)

# change these here for new data
seg_num <- '902'
seg_yr <- '2010'

# segment shapefile
seg_shp <- shps[[paste0('seg_', seg_num, '.shp')]]

# segment seagrass depth points
seg_pts <- shps[[paste0('sgpts_', seg_yr, '_', seg_num, '.shp')]]

# SpatialPointsDataFrame of secchi data
data(secc_seg)

##
# run secchi_doc function with data
rads <- seq(0.01, 0.2, length = 50)
out_rads <- vector('list', length(rads))
for(rad in seq_along(rads)){
  
  # process, remove segment shapefile, add to output
  tmp <- secc_doc(secc_seg, seg_pts, seg_shp, radius = rads[rad], seg_yr, trace = T)
  tmp$seg_shp <- NULL
  out_rads[[rad]] <- tmp
  
}

##
# create plots

mod_txt <- function(mod_in, round_val = 3){
 
  slo <- round(coef(mod_in)['SD'], round_val)
  dif <- round((-log(0.2)/1.7) - slo, round_val)
  fit <- round(summary(mod_in)$r.squared, round_val)
  out <- paste0('Slope ', slo, '\nDifference ', dif, '\nR2 ', fit)
  return(out)
  
}

x_lab <- 'Secchi depth (m)'
y_lab <- 'Seagrass depth of colonization (m)'
x_lim <- c(0, 3)
y_lim <- x_lim

pdf('C:/Users/mbeck/Desktop/OTB.pdf', family = 'serif', width = 9, height = 5)
  
for(i in seq_along(rads)){
  
  tmp_dat <- out_rads[[i]]

  ave_plo <- tmp_dat$ave_dat
  # ave_plo <- dplyr::filter(ave_plo, SD > 0.5)
  ave_mod <- lm(zmax_all ~ 0 + SD, data = ave_plo)
  near_plo <- tmp_dat$near_dat
  # near_plo <- dplyr::filter(near_plo, SD > 0.5)
  near_mod <- lm(zmax_all ~ 0 + SD, data = near_plo)

  par(mfrow = c(1, 2))
  plot(zmax_all ~ SD, ave_plo, xlab = x_lab, ylab = y_lab, 
    main = 'Averaged secchi all dates', 
    xlim = x_lim, ylim = y_lim)
  abline(ave_mod)
  legend('bottomright', mod_txt(ave_mod), bty = 'n', adj = c(0, 0))
  plot(zmax_all ~ SD, near_plo, xlab = x_lab, ylab = y_lab, 
    main = 'Nearest secchi date',
    xlim = x_lim, ylim = y_lim)
  abline(near_mod)
  legend('bottomright', mod_txt(near_mod), bty = 'n', adj = c(0, 0))
  
  title(paste0('Comparison to slope -ln(0.2)/1.7, radius ', round(rads[i], 3)), outer = T, line = -1)
  
  
}

dev.off()
  
######
# for all of Tampa Bay

# tb polygon segment
tb_seg <- readRDS('data/tb_seg.rds')

# tb secchi data
data(secc_tb)

# tb seagrass points
sgpts_2010_tb <- readRDS('data/sgpts_2010_tb.rds')

##
# run secchi_doc function with data
rads <- seq(0.01, 0.2, length = 50)
# out_rads <- vector('list', length(rads))
cl <- makeCluster(7)
registerDoParallel(cl)

# process
out_rads <- foreach(rad = seq_along(rads)) %dopar% {
  
  # process, remove segment shapefile, add to output
  tmp <- secc_doc(secc_tb, sgpts_2010_tb, tb_seg, radius = rads[rad], '2010', trace = T)
  tmp$seg_shp <- NULL
  # out_rads[[rad]] <- tmp
  tmp
  
}

##
# create plots

mod_txt <- function(mod_in, round_val = 3){
 
  slo <- round(coef(mod_in)['SD'], round_val)
  dif <- round((-log(0.2)/1.7) - slo, round_val)
  fit <- round(summary(mod_in)$r.squared, round_val)
  out <- paste0('Slope ', slo, '\nDifference ', dif, '\nR2 ', fit)
  return(out)
  
}

x_lab <- 'Secchi depth (m)'
y_lab <- 'Seagrass depth of colonization (m)'
x_lim <- c(0, 5)
y_lim <- x_lim

pdf('C:/Users/mbeck/Desktop/TampaBay.pdf', family = 'serif', width = 9, height = 5)
  
for(i in seq_along(rads)){
  
  tmp_dat <- out_rads[[i]]

  ave_plo <- tmp_dat$ave_dat
  ave_plo <- dplyr::filter(ave_plo, SD <= 3)
  ave_mod <- lm(zmax_all ~ 0 + SD, data = ave_plo)
  near_plo <- tmp_dat$near_dat
  near_plo <- dplyr::filter(near_plo, SD <= 3)
  near_mod <- lm(zmax_all ~ 0 + SD, data = near_plo)

  par(mfrow = c(1, 2))
  plot(zmax_all ~ SD, ave_plo, xlab = x_lab, ylab = y_lab, 
    main = 'Averaged secchi all dates', 
    xlim = x_lim, ylim = y_lim)
  abline(ave_mod)
  legend('bottomright', mod_txt(ave_mod), bty = 'n', adj = c(0, 0))
  plot(zmax_all ~ SD, near_plo, xlab = x_lab, ylab = y_lab, 
    main = 'Nearest secchi date',
    xlim = x_lim, ylim = y_lim)
  abline(near_mod)
  legend('bottomright', mod_txt(near_mod), bty = 'n', adj = c(0, 0))
  
  title(paste0('Comparison to slope -ln(0.2)/1.7, radius ', round(rads[i], 3)), outer = T, line = -1)
  
}

dev.off()
  