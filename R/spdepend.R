
load('data/shps.RData')

source('R/funcs.R')

# values to iterate
spc_val <- 0.02
seg_val <- '902'
seed_val <- 1234

sgpts_nm <- grep(paste0('^sgpts.*', seg_val, '\\.shp'), names(shps))
seg_nm <- paste0('seg_', seg_val, '.shp') 
sgdep <- shps[[names(shps)[sgpts_nm]]]
seg <- shps[[seg_nm]]
  
# get grid and estimates using same starting location each grid
set.seed(seed_val)
smps <- grid_est(seg, spacing = spc_val)
# point from random points for buffer
rand <- 4 #sample(1:length(smps), 1)
test_pt <- smps[rand, ]

rads <- seq(0.01, 0.2, length = 100)
out_ls <- vector('list', length = length(rads))
names(out_ls) <- as.character(rads)
for(rad in rads){
  
  # counter
  cat(which(rad == rads), 'of', length(rads), '\n')
  
  # get bathym points around test_pt
  buff_pts <- buff_ext(sgdep, test_pt, buff = rad)

  # estimate
  ests <- doc_est(buff_pts)

  # mc sens analysis
  sens_val <- sens.doc(ests, trace = F)
  
  # organize results for output
  get_vals <- c('sg_max', 'doc_med', 'doc_max')
  lower_est <- unlist(attr(sens_val, 'lower_est')[get_vals])
  act_est <- unlist(attributes(sens_val)[get_vals])
  upper_est <- unlist(attr(sens_val, 'upper_est')[get_vals])
  
  # output
  out <- rbind(lower_est, act_est, upper_est)
  out_ls[[as.character(rad)]] <- out
  
}

names(out_ls) <- rads
to_plo <- melt(out_ls)
names(to_plo) <- c('conf', 'est', 'value', 'buffer')
to_plo$buffer <- as.numeric(as.character(to_plo$buffer))

ggplot(to_plo, aes(x = buffer, y = value, group = conf, colour = conf)) + 
  geom_line() + 
  facet_wrap(~est)

##

# get bathym points around test_pt
rad <- 0.1
buff_pts <- buff_ext(sgdep, test_pt, buff = rad)

# estimate
ests <- doc_est(buff_pts)
plot.doc(ests)



