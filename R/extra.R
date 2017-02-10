source('R/funcs.r')
library(dplyr)
library(tidyr)
library(maptools)
library(foreach)
library(doParallel)

######
# irl light requirements, z_cmed

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
proc <- secc_doc(secc_all, sgpts_2009_irl, irl_seg, radius = 0.15, z_est = 'z_cmed', trace = T)
dat <- na.omit(proc)

#remove bad secchi values
dat <- dat[dat$light > 4, ] 

irl_light <- dat
save(irl_light, file = 'data/irl_light.RData')

######
# irl light requirements, z_cmax

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
proc <- secc_doc(secc_all, sgpts_2009_irl, irl_seg, radius = 0.15, z_est = 'z_cmax', trace = T)
dat <- na.omit(proc)

#remote bad secchi values and low conf
dat <- dat[dat$light > 4, ] 

irl_light_zcmax <- dat
save(irl_light_zcmax, file = 'data/irl_light_zcmax.RData')

