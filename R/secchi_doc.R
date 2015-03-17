######
# eval of seagrass doc with secchi depth

######
# for all of Tampa Bay

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

# percent requirements
reqs <- seq(0.05, 0.45, by= 0.05)
reqs <- -1 * log(reqs)/1.7

pdf('C:/Users/mbeck/Desktop/TampaBay.pdf', family = 'serif', width = 9, height = 5)
  
for(i in seq_along(rads)){
  
  tmp_dat <- out_rads[[i]]

  ave_plo <- tmp_dat$ave_dat
  ave_plo <- dplyr::filter(ave_plo, SD <= 4)
  ave_mod <- lm(z_cmax_all ~ 0 + SD, data = ave_plo)
  near_plo <- tmp_dat$near_dat
  near_plo <- dplyr::filter(near_plo, SD <= 4)
  near_mod <- lm(z_cmax_all ~ 0 + SD, data = near_plo)

  par(mfrow = c(1, 2))
  plot(z_cmax_all ~ SD, ave_plo, xlab = x_lab, ylab = y_lab, 
    main = 'Averaged secchi all dates', 
    xlim = x_lim, ylim = y_lim)
  abline(ave_mod)
  legend('bottomright', mod_txt(ave_mod), bty = 'n', adj = c(0, 0))
  plot(z_cmax_all ~ SD, ave_plo, xlab = x_lab, ylab = y_lab, 
    main = 'Light requirements', 
    xlim = x_lim, ylim = y_lim)
  sapply(reqs, function(x) abline(a = 0, b = x, lty = 2))

  title('Tampa Bay', outer = T, line = -1)
  
  
}

dev.off()

# 0.07 radius was chosen based on eval of plots created from above for all of Tampa Bay

######
# for all of IRL

# irl polygon segment
data(irl_seg)

# all secchi data
data(secc_all)

# tb seagrass points
data(sgpts_2009_irl)

##
# subset secc_all to within ten years of sg data
yrs <- strftime(secc_all$Date, '%Y') %>% 
  as.numeric
yrs <- yrs <= 2009 & yrs > 1999
secc_all <- secc_all[yrs, ]

##
# run secchi_doc function with data
rads <- seq(0.01, 0.1, length = 50)

cl <- makeCluster(7)
registerDoParallel(cl)

# process
out_rads <- foreach(rad = seq_along(rads)) %dopar% {
  
  # process, remove segment shapefile, add to output
  tmp <- secc_doc(secc_all, sgpts_2009_irl, irl_seg, radius = rads[rad], '2009', 
    trace = T)
  tmp$seg_shp <- NULL
  # out_rads[[rad]] <- tmp
  tmp
  
}

##
# create plots

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

# percent requirements
reqs <- seq(0.05, 0.45, by= 0.05)
reqs <- -1 * log(reqs)/1.7

pdf('C:/Users/mbeck/Desktop/IRL.pdf', family = 'serif', width = 9, height = 5)
  
for(i in seq_along(rads)){
  
  tmp_dat <- out_rads[[i]]

  ave_plo <- tmp_dat$ave_dat
  # ave_plo <- dplyr::filter(ave_plo, SD <= 4)
  ave_mod <- lm(z_cmax_all ~ 0 + SD, data = ave_plo)
  near_plo <- tmp_dat$near_dat
  # near_plo <- dplyr::filter(near_plo, SD <= 4)
  near_mod <- lm(z_cmax_all ~ 0 + SD, data = near_plo)

  par(mfrow = c(1, 2))
  plot(z_cmax_all ~ SD, ave_plo, xlab = x_lab, ylab = y_lab, 
    main = 'Averaged secchi all dates', 
    xlim = x_lim, ylim = y_lim)
  abline(ave_mod)
  legend('bottomright', mod_txt(ave_mod), bty = 'n', adj = c(0, 0))
  plot(z_cmax_all ~ SD, ave_plo, xlab = x_lab, ylab = y_lab, 
    main = 'Light requirements', 
    xlim = x_lim, ylim = y_lim)
  sapply(reqs, function(x) abline(a = 0, b = x, lty = 2))

  title('Indian River Lagoon', outer = T, line = -1)
  
}

dev.off()

# radius of 0.02 was chosen, don't know why

######
# mean euclidean distance between each points nearest neighbor

rm(list = ls())

# to get nearest neighbor distances
library(FNN)

# load data
data(secc_all)
data(irl_seg)
data(tb_seg)

## 
# add yrs
secc_all$yr <- as.numeric(strftime(secc_all$Date, '%Y'))

##
# subset tb lat/long, within ten years (2000 to 2010)
sel <- !is.na(secc_all %over% tb_seg)[, 1]
sel <- sel & secc_all$yr <= 2010 & secc_all$yr > 2000
secc <- data.frame(secc_all[sel, ])
secc <- unique(secc[c('Longitude', 'Latitude')])

# distance between nearest neighbs
tb_nn <- get.knn(secc, k = 1)
mean(tb_nn$nn.dist)

# mean of all dists
mean(dist(secc))

# mean distance between neighboring points is 0.02282

## 
# subset irl lat/long, within ten years (1999 to 2009)
sel <- !is.na(secc_all %over% irl_seg)[, 1]
sel <- sel & secc_all$yr <= 2009 & secc_all$yr > 1999
secc <- data.frame(secc_all[sel, ])
secc <- unique(secc[c('Longitude', 'Latitude')])

# distance between nearest neighbs
irl_nn <- get.knn(secc, k = 1)
mean(irl_nn$nn.dist)

# mean of all dists
mean(dist(secc))

# mean distance between neighboring points is 0.01332

######
# light requirements for individual wbids

# data to use
data(shps)
data(secc_seg)

# segs and years to process
to_proc <- list(
  c('303', '2007'), 
  c('902', '2010'),
  c('1502', '2009')
)

cl <- makeCluster(7)
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
    ave_mod <- lm(z_cmax_all ~ 0 + SD, data = ave_plo)
    near_plo <- tmp_dat$near_dat
    # near_plo <- dplyr::filter(near_plo, SD > 0.5)
    near_mod <- lm(z_cmax_all ~ 0 + SD, data = near_plo)
  
    par(mfrow = c(1, 2))
    plot(z_cmax_all ~ SD, ave_plo, xlab = x_lab, ylab = y_lab, 
      main = 'Averaged secchi all dates', 
      xlim = x_lim, ylim = y_lim)
    abline(ave_mod)
    legend('bottomright', mod_txt(ave_mod), bty = 'n', adj = c(0, 0))
    plot(z_cmax_all ~ SD, near_plo, xlab = x_lab, ylab = y_lab, 
      main = 'Nearest secchi date',
      xlim = x_lim, ylim = y_lim)
    abline(near_mod)
    legend('bottomright', mod_txt(near_mod), bty = 'n', adj = c(0, 0))
    
    title(paste0('Segment ', to_proc[[val]][1], ', Comparison to slope -ln(0.2)/1.7, radius ', round(rads[i], 3)), outer = T, line = -1)
    
  }
}

dev.off()

#######
# plots of contour lines for light requirements by segment

## 
# TB

data(tb_light)

to_plo <- data.frame(tb_light)

plot(z_cmax_all ~ SD, data= to_plo, pch = 16, col = factor(to_plo$seg), xlim = c(0.5, 3), ylim = c(0.5, 3))

# percent requirements
reqs <- seq(0.05, 0.45, by= 0.05)
reqs <- -1 * log(reqs)/1.7
sapply(reqs, function(x) abline(a = 0, b = x))


ggplot(to_plo, aes(x = SD, y = z_cmax_all, colour = seg)) + 
  geom_point(size = 4) + 
  theme_classic() + 
  geom_abline(intercept = 0, slope = reqs, linetype = 'dashed') +
  # geom_text(aes(label = round(light, 1))) +
  ylab('Max depth of colonization (m)') +
  xlab('Secchi ten-year average (m)') +
  theme(legend.title = element_blank())

##
# irl

data(irl_light)

to_plo <- data.frame(irl_light)

plot(z_cmax_all ~ SD, data = to_plo, pch = 16, col = factor(to_plo$seg), xlim = c(0.5, 3), ylim = c(0.5, 3))

# percent requirements
reqs <- c(seq(0.01, 0.04, by = 0.01), seq(0.05, 0.35, by= 0.05))

p1 <- ggplot(to_plo, aes(x = SD, y = z_cmax_all, colour = seg)) + 
  geom_point(size = 4) + 
  theme_classic() + 
  geom_abline(intercept = 0, slope = -1 * log(reqs)/1.7, linetype = 'dashed') +
  # geom_text(aes(label = round(light, 1))) +
  ylab('Max depth of colonization (m)') +
  xlab('Secchi ten-year average (m)') +
  theme(legend.title = element_blank())

p2 <- ggplot(to_plo, aes(x = SD, y = light, colour = seg)) + 
  geom_point(size = 4) + 
  theme_classic() + 
  geom_hline(yintercept = 100*reqs, linetype = 'dashed') +
  # geom_text(aes(label = round(light, 1))) +
  ylab('% light requirements') +
  xlab('Secchi ten-year average (m)') +
  theme(legend.title = element_blank())

p3 <- ggplot(to_plo, aes(x = z_cmax_all, y = light, colour = seg)) + 
  geom_point(size = 4) + 
  theme_classic() + 
  geom_hline(yintercept = 100*reqs, linetype = 'dashed') +
  # geom_text(aes(label = round(light, 1))) +
  xlab('Max depth of colonization (m)') +
  ylab('% light requirements') +
  theme(legend.title = element_blank())

grid.arrange(p1, p2, p3, ncol = 3)

######
# validation of kd satellite data
# Tampa Bay, by years

library(magrittr)
library(dplyr)
library(raster)
library(maptools)

# segment
data(tb_seg)

# secchi
data(secc_all)

# satellite
data(tb_sats)

# clip secchi by seg
# avg by station and yr, for only years with kd data
sel <- !is.na(secc_all %over% tb_seg)[, 1]
secc <- secc_all[sel, ]
secc <- mutate(data.frame(secc), 
    yr = as.numeric(format(Date, '%Y')), SD = as.numeric(SD)
  ) %>% 
  filter(yr <= 2010 & yr >= 2003) & yr != 2005 %>% 
  group_by(Station_ID, yr, Longitude, Latitude) %>% 
  summarise(mean_SD = mean(SD, na.rm = T)) %>% 
  data.frame

# all satellite data
sats_all <- tb_sats$sats_all

# sample kd satellite surface
outs <- NULL
for(yr_sel in unique(secc$yr)){
  
  cat(yr_sel, '\t')
  
  # get correct secchi yrs
  yr_secc <- filter(secc, yr == yr_sel)
  
  if(nrow(yr_secc) == 0) next
  
  coordinates(yr_secc) <- c('Longitude', 'Latitude')
  
  # get correct satellite yrs
  sel_vec <- grepl(paste0('_', yr_sel, '$|lon|lat'), names(sats_all))
  yr_sats <- sats_all[, sel_vec]
  sp::coordinates(yr_sats) <- c('lon', 'lat')
  
  # rasterize
  # set up raster template
  rast <- raster::raster()
  extent(rast) <- extent(yr_sats)
  ncol(rast) <- length(unique(yr_sats$lon))
  nrow(rast) <- length(unique(yr_sats$lat))

  sel_vec <- grepl(paste0('_', yr_sel, '$'), names(yr_sats))
  z_field <- data.frame(yr_sats)[, sel_vec]
  yr_sats <- rasterize(yr_sats, rast, z_field, fun = mean) 
  
  # extract kz values using secci locations for the year
  samp_vals <- raster::extract(yr_sats, yr_secc, sp = T) %>% 
    data.frame  %>% 
    na.omit
  
  names(samp_vals)[grepl('^kz_', names(samp_vals))] <- 'kz'
  
  # append output
  outs <- rbind(outs, samp_vals)
  
}

ggplot(outs, aes(x = kz, y = mean_SD, colour = factor(yr))) +
  geom_point(size = 4)

mod <- lm(mean_SD ~ kz, outs)
summary(mod)

##
# this looks at average comparisons

library(magrittr)
library(dplyr)
library(raster)
library(maptools)

# segment
data(tb_seg)

# secchi
data(secc_all)

# satellite
data(tb_sats)

sats_all <- tb_sats$sats_all
sats_ave <- sats_all[, c('lon', 'lat', 'kz_ave')]
sp::coordinates(sats_ave) <- c('lon', 'lat')

# set up raster template
rast <- raster()
extent(rast) <- extent(sats_ave)
ncol(rast) <- length(unique(sats_ave$lon))
nrow(rast) <-length(unique(sats_ave$lat))

sat_rast <- rasterize(sats_ave, rast, sats_ave$kz_ave, fun = mean) 

# clip secchi by seg
# avg by station, for only years with kd data
sel <- !is.na(secc_all %over% tb_seg)[, 1]
secc <- secc_all[sel, ]
secc <- mutate(data.frame(secc), 
    yr = as.numeric(format(Date, '%Y')), SD = as.numeric(SD)
  ) %>% 
  filter(yr <= 2010 & yr >= 2003) %>% 
  group_by(Station_ID, Longitude, Latitude) %>% 
  summarise(mean_SD = mean(SD, na.rm = T)) %>% 
  data.frame
  
coordinates(secc) <- c('Longitude', 'Latitude')

# extract
ave_rast <- tb_sats$ave_rast
samp_vals <- raster::extract(ave_rast, secc, sp = T) %>% 
  data.frame  

plot(layer ~ mean_SD, samp_vals)
mod <- lm(layer ~ 0 + recip, samp_vals)

######
# compare

library(magrittr) 
  
files <- list.files('M:/docs/sg_depth/data/satellite/Tampa_Bay/',
  pattern = '^2004', full.names = TRUE)

# get files
sats <- vector('list', length = length(files))
names(sats) <- files
for(fl in files){
  
  cat(fl, '\n')
  
  # import 
  tmp <- readLines(fl) %>% 
    strsplit(' .') %>% 
    unlist
  tmp <- tmp[nchar(tmp) > 0] %>% 
    as.numeric
  
  sats[[fl]] <- tmp
  
}
names(sats) <- gsub('\\.txt$', '', basename(names(sats)))


sats <- do.call('cbind', sats)

test <- 1.4/sats[, '2004_clarity']

par(mfrow = c(2, 1))
plot(sats[, '2004_kd'], sats[, '2004_clarity'])
plot(sats[, '2004_kd'], test)
abline(0, 1)

