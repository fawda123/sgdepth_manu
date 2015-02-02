######
# eval of seagrass doc with secchi depth

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
  tmp <- secc_doc(secc_all, sgpts_2010_tb, tb_seg, radius = rads[rad], '2010', trace = T)
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
  ave_plo <- dplyr::filter(ave_plo, SD <= 4)
  ave_mod <- lm(zmax_all ~ 0 + SD, data = ave_plo)
  near_plo <- tmp_dat$near_dat
  near_plo <- dplyr::filter(near_plo, SD <= 4)
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
# 0.07 radius was chosen based on eval of plots created from above for all of Tampa Bay

rm(list = ls())

library(ggplot2)
library(gtable)
library(gridExtra)

source('R/funcs.r')

# tb polygon segment
tb_seg <- readRDS('data/tb_seg.rds')

# all secchi data
data(secc_all)

# tb seagrass points
sgpts_2010_tb <- readRDS('data/sgpts_2010_tb.rds')

# process, get ave secchi data results
proc <- secc_doc(secc_tb, sgpts_2010_tb, tb_seg, radius = 0.07, '2010', trace = T)
dat <- na.omit(proc$ave_dat)
dat <- dplyr::filter(dat, SD <3)
dat <- dplyr::mutate(dat, 
  light = exp(-zmax_all * 1.7/SD),
  seg = NA
  )

# convert to spatialpointsdataframe
coords <- dat[, c('Longitude', 'Latitude')]
dat <- dat[, !names(dat) %in% c('Longitude', 'Latitude')]
coordinates(dat) <- coords

# get segment for each point
tb_seg <- readRDS('M:/docs/manuscripts/sgdepth_manu/data/tb_seg.rds')
for(seg in as.character(tb_seg$seg)){
  
  to_sel <- tb_seg[tb_seg$seg %in% seg, ]
  sel <- !is.na(dat %over% to_sel)[, 1]
  dat[sel, 'seg'] <- as.character(seg)
  
}

# centroids for labels
labs <- data.frame(rgeos::gCentroid(tb_seg, byid = T))
labs$seg <- c('HB', 'LTB', 'MTB', 'OTB')

fill_col <- colors()[245]

dat <- data.frame(dat)
p1 <- ggplot(fortify(tb_seg), aes(long, lat)) + 
  geom_polygon(colour = 'black', fill = fill_col, aes(group = id)) +
  geom_point(data = dat, aes(x = Longitude, y = Latitude, 
    colour = light, size = light), fill = 'black', alpha = 0.8) +
  geom_text(data = labs, aes(label = seg, x = x, y = y)) +
  theme_classic() +
  coord_equal() +
  xlab('Longitude') +
  ylab('Latitude') +
  scale_size(name = 'Light requirements', range = c(2, 9)) + 
  scale_colour_gradientn(name = 'Light requirements', 
    colours = c('blue', 'lightblue', 'green')) + #rev(brewer.pal(9,  'BuGn'))) +
  guides(colour = guide_legend(), size = guide_legend()) + 
  theme(
    legend.position = c(1, 0),
    legend.justification = c(1, 0))

p2 <- ggplot(data.frame(dat), aes(x = seg, y = light, size = light)) + 
  geom_boxplot(fill = fill_col) + 
  ylab('Light requirements') +
  theme_classic() +
  theme(axis.title.y = element_blank()) + 
  coord_flip()

grid.arrange(p1, p2, nrow = 1, widths = c(1, 0.5))

######
# for all of IRL

# irl polygon segment
irl_seg <- readRDS(file = 'data/irl_seg.rds')

# all secchi data
data(secc_all)

# tb seagrass points
sgpts_2009_irl <- readRDS('data/sgpts_2009_irl.rds')

##
# run secchi_doc function with data
rads <- seq(0.01, 0.2, length = 50)

# cl <- makeCluster(7)
# registerDoParallel(cl)
# 
# # process
# out_rads <- foreach(rad = seq_along(rads)) %dopar% {
#   
#   # process, remove segment shapefile, add to output
#   tmp <- secc_doc(secc_all, sgpts_2009_irl, irl_seg, radius = rads[rad], '2009', 
#     trace = T)
#   tmp$seg_shp <- NULL
#   # out_rads[[rad]] <- tmp
#   tmp
#   
# }
# 
# res_irl <- out_rads
# save(res_irl, file = 'data/res_irl.RData')

##
# create plots

data(res_irl)
out_rads <- res_irl

mod_txt <- function(mod_in, round_val = 3){
 
  slo <- round(coef(mod_in)['SD'], round_val)
  dif <- round((-log(0.2)/1.7) - slo, round_val)
  fit <- round(mean(resid(mod_in)^2), 3)
  out <- paste0('Slope ', slo, '\nDifference ', dif, '\nMSE ', fit)
  return(out)
  
}

x_lab <- 'Secchi depth (m)'
y_lab <- 'Seagrass depth of colonization (m)'
x_lim <- c(0, 3)
y_lim <- x_lim

pdf('C:/Users/mbeck/Desktop/IRL.pdf', family = 'serif', width = 9, height = 5)
  
for(i in seq_along(rads)){
  
  tmp_dat <- out_rads[[i]]

  ave_plo <- tmp_dat$ave_dat
  # ave_plo <- dplyr::filter(ave_plo, SD <= 4)
  ave_mod <- lm(zmax_all ~ 0 + SD, data = ave_plo)
  near_plo <- tmp_dat$near_dat
  # near_plo <- dplyr::filter(near_plo, SD <= 4)
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
# for individual wbids

# data to use
data(shps)
data(secc_seg)

# segs and years to process
to_proc <- list(
  c('303', '2007'), 
  c('902', '2010'),
  c('1502', '2009')
)

cl <- makeCluster(4)
registerDoParallel(cl)

res <- foreach(i = seq_along(to_proc)) %dopar% {
  
  seg <- to_proc[[i]][1]
  yr <- to_proc[[i]][2]
  
  # segment shapefile
  seg_shp <- shps[[paste0('seg_', seg, '.shp')]]
  
  # segment seagrass depth points
  seg_pts <- shps[[paste0('sgpts_', yr, '_', seg, '.shp')]]  
  
  ##
  # run secchi_doc function with data
  rads <- seq(0.01, 0.2, length = 50)
  out_rads <- vector('list', length(rads))
  for(rad in seq_along(rads)){
     
    sink('C:/Users/mbeck/Desktop/log.txt')
    cat(seg, rad, '\n')
    sink()
    
    # process, remove segment shapefile, add to output
    tmp <- secc_doc(secc_seg, seg_pts, seg_shp, radius = rads[rad], yr, trace = F)
    tmp$seg_shp <- NULL
    out_rads[[rad]] <- tmp
    
  }
  
  out_rads
  
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

pdf('C:/Users/mbeck/Desktop/secc_segs.pdf', family = 'serif', width = 9, height = 5)

for(val in seq_along(res)){
  for(i in seq_along(rads)){
  
    res_val <- res[[val]]
    tmp_dat <- res_val[[i]]
  
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
    
    title(paste0('Segment ', to_proc[[val]][1], ', Comparison to slope -ln(0.2)/1.7, radius ', round(rads[i], 3)), outer = T, line = -1)
    
  }
}

dev.off()