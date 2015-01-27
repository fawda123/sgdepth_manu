######
# eval of seagrass doc with secchi depth

# data 
data(shps)
seg_shp <- shps[['seg_902']]
data(sgpts_2010_902)
data(secc_seg)

rad <- 0.07
tmp <- secc_doc(seg_shp, sgpts_shp, radius = rad, trace = T)


##@#@#$234234234


# plot(tmp$seg_shp)
# with(tmp$ave_dat, points(Longitude, Latitude)) 
# 
# plot(zmax_all ~ SD, data = tmp$ave_dat)
# 
# plot(table(strftime(tmp$all_dat$Date, '%Y')))

near_date <- tmp$all_dat
near_date <- split(near_date, near_date$Station_ID)
near_date <- llply(
  near_date, 
  .fun = function(x){
    
    x <- x[which.max(x$Date), ]
    x$diff <- 2010 - as.numeric(strftime(x$Date, '%Y'))
    x
    
  })
near_date <- do.call('rbind', near_date)
near_date$wts <- scales::rescale(near_date$diff, to = c(0.5, 7))
near_date$diff <- scales::rescale(near_date$diff, to = c(1, 3))

par(mfrow = c(1, 2))

plot(zmax_all ~ SD, near_date, cex = near_date$diff, pch = 16, main = 'Nearest', ylim = c(0, 2))
mod <- lm(zmax_all ~ as.numeric(SD), near_date)
wt_mod <- lm(zmax_all ~ as.numeric(SD), near_date, weights = near_date$wts)
abline(reg = mod, lty =  2)
abline(reg = wt_mod)

plot(zmax_all ~ SD, tmp$ave_dat, pch = 16, main = 'Averaged', ylim = c(0,2))
mod <- lm(zmax_all ~ as.numeric(SD), tmp$ave_dat)
abline(reg = mod)

#####
# all data for TB

load('seagrass_gis/shps.RData')
seg_shp <- shps[[grep('seg_902', names(shps))]]
sgpts <- shps[grep('^sgpts.*902\\.shp$', names(shps))]

rad <- 0.07

out_ls <- vector('list', length(sgpts))
names(out_ls) <- names(sgpts)
for(sgpt in names(sgpts)){
  cat(sgpt, '\n')
  tmp <- secc_doc(seg_shp, sgpts[[sgpt]], radius = rad, trace = T)
  out_ls[[sgpt]] <- tmp[[2]]
}
