######
# functions for seagrass depth of col estimates

######
#' Create a doc object
#' 
#' Wrapper for creating a doc object
#' 
#' @param  ls_in list input created internally within \code{\link{doc_est}}
#' 
#' @export doc
#' 
#' @return Returns a doc object to be used with S3 methods
#' 
#' @details 
#' This function is a simple wrapper to \code{\link[base]{structure}} that is used internally within other functions to create a doc object.  The function does not have to be used explicitly.    
#' 
doc <- function(ls_in){
    
  # sanity check
  if(any(!names(ls_in) %in% c('data', 'preds', 'logis_mod', 'est_fun', 'sg_max', 'doc_med', 'doc_max', 'lower_est', 'upper_est'))) stop('Incorrect input for doc object')

  # create class, with multiple attributes
  structure(
    .Data = ls_in[['data']], 
    class = c('doc', 'data.frame'), 
    preds = ls_in$preds,
    logis_mod = ls_in$logis_mod, 
    est_fun = ls_in$est_fun,
    sg_max = ls_in$sg_max,
    doc_med = ls_in$doc_med, 
    doc_max = ls_in$doc_max,
    lower_est = NA,
    upper_est = NA
    )
  
}


######
# formatting of values in S expressions
form_fun <- function(x, rnd_val = 2, dig_val = 2, nsm_val = 2) {
  to_form <- as.numeric(as.character(x))
  format(round(to_form, rnd_val), digits = dig_val, nsmall = nsm_val)
  }

######
# get logistic model from input
# dat_in is data frame of empirical estimates of seagrass occupancy by depth
# resp is name of response variable, usually sg_prp
# new_val is number of obs for predicting from fitted mod
logis_est <- function(dat_in, resp = 'sg_prp', new_vals = 500){
  
  pts <- dat_in
  
	# logistic growth
	Asym <- max(pts[, resp], na.rm = T)
	xmid <- median(pts$Depth, na.rm = T)
	scal <- quantile(pts$Depth, 0.75, na.rm = T) - xmid
	form_in <- substitute(x ~ SSlogis(Depth, Asym,  xmid, scal), 
    list(x = as.name(resp)))

	# model
	mod <- try({nls(form_in, data = pts, na.action = na.exclude)}, silent = T)
	
  # values for prediction
	dep_rng <- range(pts[, 'Depth'], na.rm = T)
	new.x <- seq(dep_rng[1], dep_rng[2], length = new_vals)

  # return NAs if model fail, else get model predictions
  if('try-error' %in% class(mod)) {
    pred <- rep(NA_real_, length = length(new.x))
    asym <- NA_real_
    logis_mod <- NA
    message('Cannot fit curve to data\n')
  } else {
	  pred <- as.numeric(predict(mod, 
		  newdata = data.frame(Depth = new.x)))
    pred <- data.frame(new.x, pred)
    names(pred) <- c('Depth', resp)
    asym <- summary(mod)$coefficients['Asym', 'Estimate']
    logis_mod <- mod
  }
  
  # return output
  out <- list(pred = pred, asym = asym, logis_mod = logis_mod)
  return(out)
  
}

######
# get doc ests from fitted logistic regression curve
# dat_in is two column data frame first is depth, second is predicted proportion of points occupied by seagrass
# asym is asymptote estimate from logistic mod
get_ests <- function(dat_in, asym){
  
  if(class(dat_in) != 'data.frame') stop('Input must be data frame')
  
  # exp and resp columns from dat_in
  x_val <- dat_in[, 1]
  y_val <- dat_in[, 2]
  
  # first deriv
  inflect <- diff(y_val)/diff(x_val)
  ind_min <- which.min(inflect)
    
  est_fun <- NA
  sg_max <- NA
  doc_med <- NA
  doc_max <- NA
  
  # get curve estimate if the minimum slope is not the last value
  if(ind_min != (nrow(dat_in) - 1)){
    
    inflect_val <- dat_in[ind_min + 1, ]
    slope_val <- inflect[ind_min]
    int_val <- inflect_val[, 2] - slope_val * inflect_val[, 1]
    est_fun <- function(x) slope_val * x + int_val
    doc_max <- -1 * int_val / slope_val
    
    # get doc_med, halfway between sg_max and doc_max
    # sg_max is based on asymptote intercept with linear reg
    # sg_max defaults to zero if value is extrapolated
    sg_max <- max(c(0, (asym - int_val)/slope_val))
    doc_med  <- sg_max + ((doc_max - sg_max)/2)

  }
  
  # output
  out <- list(preds = dat_in, est_fun = est_fun, sg_max = sg_max, doc_med = doc_med, doc_max = doc_max)
  return(out)
  
}

######
# function for estimating depth of colonization
# also used for plots
# 'dat_in' is data from 'buff_ext'
# 'depth_var' is name of depth column in input data
# 'sg_var' is name of seagrass column in input data
doc_est <- function(dat_in, depth_var = 'Depth', sg_var = 'Seagrass', sg_cat = c('Continuous', 'Discontinuous')){
  
  # sanity check
  if(!'data.frame' %in% class(dat_in)) dat_in <- data.frame(dat_in)
  
	# order by depth, assumes column is negative
  dat_in <- dat_in[order(dat_in[, depth_var], decreasing = T), ]
	dat_in$depth <- dat_in[, depth_var]
  
  # bin depth values
  dat_in[, depth_var] <- round(dat_in[, depth_var], 1)
	
	# cumulative sum of pts with all seagrass and all points
	# assumes NA is empty
	sg_pts <- table(dat_in[dat_in[, sg_var] %in% sg_cat, depth_var])
  
  # stop function if no seagrass found
  if(length(sg_pts) == 0) stop('No seagrass present')
  
	sg_pts <- data.frame(Depth = names(sg_pts), sg_pts = as.numeric(sg_pts),
		row.names = 1:length(sg_pts))
	dep_pts <- table(dat_in[, depth_var])
	dep_pts <- data.frame(Depth = names(dep_pts), dep_pts = as.numeric(dep_pts), 
		row.names = 1:length(dep_pts))
	
	# combine all pts and seagrass pts, depth as numeric
	pts <- merge(dep_pts, sg_pts, by = 'Depth', all.x = T)
	pts$Depth <- as.numeric(as.character(pts$Depth))
	# output
  pts$sg_prp <- with(pts, sg_pts/dep_pts)
	
	##
	# estimate a logistic growth function for the data
  mod_est <- logis_est(pts, 'sg_prp')

  # output
	preds <- mod_est$pred
	asym <- mod_est$asym
  logis_mod <- mod_est$logis_mod
  
  ##
	# calculate depth of col using get_est

  # if no curve fit
  if(any(is.na(logis_mod))){
    
    # create doc output
    ls_in <- list(data = pts, preds = preds, logis_mod = logis_mod, est_fun = NA, 
      sg_max = NA, doc_med = NA, doc_max = NA, lower_est = NA, upper_est = NA)
    
    out <- doc(ls_in)
    return(out)
     
  }
  
  # check if curve is monotonic descending
  if(!with(preds, all(sg_prp == cummin(sg_prp)))){
    
    ls_in <- list(data = pts, preds = preds, logis_mod = logis_mod, est_fun = NA, 
      sg_max = NA, doc_med = NA, doc_max = NA, lower_est = NA, upper_est = NA)
    
    out <- doc(ls_in)
    return(out)
    
  }
  
  # get doc estimates using get_ests functions
  ests <- get_ests(preds, asym)
  preds <- ests[['preds']]
  est_fun <- ests[['est_fun']]
  sg_max <- ests[['sg_max']]
  doc_med <- ests[['doc_med']]
  doc_max <- ests[['doc_max']]
    
  # all output
  ls_in <- list(data = pts, preds = preds, logis_mod = logis_mod, est_fun = est_fun, 
    sg_max = sg_max, doc_med = doc_med, doc_max = doc_max, lower_est = NA, 
    upper_est = NA)
  
  out <- doc(ls_in)
  return(out)
    
}

######
# create a plot of doc estimates from doc_est, same inputs as doc_est
# 'dat_in' is data from 'buff_ext'
# 'depth_var' is name of depth column in input data
# 'sg_var' is name of seagrass column in input data
plot.doc <- function(doc_in, sens = F){
  
  to_plo <- data.frame(doc_in)
  ests <- attributes(doc_in)[c('sg_max', 'doc_med', 'doc_max')]
  est_fun <- attr(doc_in, 'est_fun')
  
  # base plot if no estimate is available
  p <- ggplot(to_plo, aes(x = Depth, y = sg_prp)) +
    geom_point(pch = 1, size = 4) +
    theme(text = element_text(size=20)) +
    ylab('Proportion of points with seagrass') +
    xlab('Depth (m)')

  # get y value from est_fun for sg_max and doc_med
  yends <- try({
    with(attributes(doc_in), est_fun(c(sg_max, doc_med)))
    }, silent = T)
  
  # add to baseplot if estimate is available
  if(!'try-error' %in% class(yends)){
  
    ##
		# simple plot of points by depth, all pts and those with seagrass
    to_plo2 <- attr(doc_in, 'pred')
    to_plo3 <- attr(doc_in, 'est_fun')
    to_plo4 <- data.frame(
      Depth = unlist(ests), 
      yvals = rep(0, 3)
    )
    
    # some formatting crap
    x_lims <- max(1.1 * max(na.omit(to_plo)$Depth), 1.1 * ests$doc_max)
    pt_cols <- brewer.pal(nrow(to_plo4), 'Blues')
    leg_lab <- paste0(
      c('SG max (', 'DOC med (', 'DOC max ('),
      round(to_plo4$Depth, 2), 
      rep(')', 3)
    )
    
    # the plot
    p <- p +
      geom_line(data = to_plo2, 
        aes(x = Depth, y = sg_prp)
        ) +
      scale_y_continuous(limits = c(0, 1.1 * max(to_plo2$sg_prp))) + 
      scale_x_continuous(limits = c(min(to_plo$Depth), 1.1 * x_lims)) + 
      stat_function(fun = to_plo3, colour = 'lightgreen', size = 1.5, 
        alpha = 0.6) +
      geom_segment(x = ests$sg_max, y = 0, xend = ests$sg_max, 
        yend = yends[1], linetype = 'dashed', colour = 'lightgreen',
        size = 1.5, alpha = 0.6) +
      geom_segment(x = ests$doc_med, y = 0, xend = ests$doc_med, 
        yend = yends[2], linetype = 'dashed', colour = 'lightgreen',
        size = 1.5, alpha = 0.6) +
      geom_point(data = to_plo4, 
        aes(x = Depth, y = yvals, fill = factor(Depth)), 
        size = 6, pch = 21) +
      scale_fill_brewer('Depth estimate (m)', 
        labels = leg_lab,
        palette = 'Blues'
        ) +
      theme(legend.position = c(1, 1),
        legend.justification = c(1, 1)) 
    
    # add upper, lower estimates to plot
    if(sens){
      
      # run sensitivity analysis if not present
      if(!'data.frame' %in% class(attr(doc_in, 'lower_est')$preds)) 
        doc_in <- sens(doc_in)
      
      lower_est <- attr(doc_in, 'lower_est')
      upper_est <- attr(doc_in, 'upper_est')
      p <- p +
        geom_line(data = lower_est$preds, aes(y = sg_prp), colour = 'tomato') +
        geom_line(data = upper_est$preds, aes(y = sg_prp), colour = 'tomato')
      
    }
    
  }
  
  p
  
}
 
#######
# function for creating random grid of points, bounded by polygon extent
# taken from ibi sampling manuscript functions
# 'clip_poly' is shapefile input object
# 'spacing' is spacing between points, as degrees
grid_est <- function(clip_poly, spacing = 0.02){
  
  if(!'SpatialPolygonsDataFrame' %in% class(clip_poly))
    stop('clip_poly must be of class SpatialPolygonsDataFrame')
  
  library(sp) 
  
  # extent of shapefile
  extent <- summary(clip_poly)$bbox
  
  # buffer of shapefile and random starting x/y locs
  add.on <- apply(extent, 1, diff) * 0.3
  rand <- runif(2, 0, spacing)
  
  # random points within rectangular extent
  pts<-{
    x.vals<-seq(extent[1, 1] - add.on['x'], extent[1, 2] + add.on['x'], by = spacing) + rand[1]
    y.vals<-seq(extent[2, 1] - add.on['y'], extent[2, 2] + add.on['y'], by = spacing) + rand[2]
    expand.grid(x.vals, y.vals)
  }
  
  # clip by clip_poly and return
  sel <- !is.na(SpatialPoints(pts) %over% clip_poly)[, 1]
  
  return(SpatialPoints(pts)[sel, ])
  
}

######
# function extracts bathymetric seagrass pts withing a distance from a pt
# 'pts' is spatial points to extract
# 'center' is pt from which buffer extends
# 'buff' is radius of buffer in dec degrees
buff_ext <- function(pts, center, buff = 0.03){
  
  # sanity checks
  if(!any(c('SpatialPointsDataFrame', 'SpatialPoints') %in% class(pts)))
    stop('pts must be of class SpatialPointsDataFrame or SpatialPoints')
  
  if(!any(c('SpatialPointsDataFrame', 'SpatialPoints') %in% class(center)))
    stop('center must be of class SpatialPointsDataFrame or SpatialPoints')
  
  library(rgeos)
  library(sp)
  
  # create buffer
  buffer <- gBuffer(center, width = buff)
  
  # index of pts in buffer
  sel <- !is.na(pts %over% buffer)
  
  if(sum(sel) == 0) stop('No points in buffer')
  
  return(pts[sel, ])
  
}

######
#' get seagrass depth estimates for an entire sample grid
#'
#' @param grid_in SpatialPoints object of locations to estimate seagrass depth, created using \code{\link{grid_est}}
#' @param dat_in SpatialPointsDataFrame of seagrass depth points for sampling with \code{grid_in}
#' @param buff radius of buffer in dec degrees around each sample location for estimating seagrass depth estimates
#' @param rem_miss logical indicating if unestimable points are removed from the output, default \code{TRUE}
#' @param trace logical indicating if progress is returned in console, default \code{FALSE}
#' 
#' @details This function estimates three seagrass depth of colonization values for each point in a sampling grid.  Functions \code{\link{buff_ext} and \code{\link{doc_est}} are used iteratively for each point in the sample grid.
#' 
#' @import sp
#' 
#' @return 
doc_est_grd <- function(grid_in, dat_in, radius = 0.06, rem_miss = TRUE, trace = FALSE){
      
  # get estimates for each point
  maxd <- vector('list', length = length(grid_in))
  for(i in 1:length(grid_in)){
    
    if(trace) cat(i, 'of', length(grid_in), '\n')
      
    eval_pt <- grid_in[i, ]
    ests <- try({
      buff_pts <- buff_ext(dat_in, eval_pt, buff = radius)
  	  est_pts <- data.frame(buff_pts)
      doc_single <- attributes(est_pts)[c('sg_max', 'doc_med', 'doc_max')]
      unlist(doc_single)
    }, silent = T)
    
  	if('try-error' %in% class(ests)) ests <- rep(NA, 3)
    
    maxd[[i]] <- ests
    
  }
    
	# combine results in data_frame, convert to spatialpoints
	maxd <- data.frame(do.call('rbind', maxd)) 
  maxd <- sp::SpatialPointsDataFrame(coords = coordinates(grid_in), data = maxd)
  
  # remove missing
  if(rem_miss){
    non_empty <- !is.na(maxd@data[, 1])
    if(!any(non_empty)) maxd
    else maxd <- maxd[!is.na(maxd@data[, 1]), ]
  }
 
  return(maxd)
  
}

######
# get legend from an existing ggplot object
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

######
# get confidence intervals for doc object using MC sims
# modified from...
# http://rmazing.wordpress.com/2013/08/14/predictnls-part-1-monte-carlo-simulation-confidence-intervals-for-nls-models/
sens <- function(doc_in, ...) UseMethod('sens')
sens.doc <- function(doc_in, newdata, level = 0.95, nsim = 5000, trace = T, ...){
   
  # use object attribute if not supplied
  if(missing(newdata))
    newdata <- attr(doc_in, 'pred')[, 'Depth', drop = F]

  mod <- attr(doc_in, 'logis_mod')
  if(!'nls' %in% class(mod)) stop('No model in object')
  
  # extract predictor variable name   
  predNAME <- names(newdata)
   
  # get parameter coefficients from model
  COEF <- coef(mod)
     
  ## get variance-covariance matrix from model
  VCOV <- vcov(mod)
   
  # augment variance-covariance matrix for 'mvrnorm' 
  # by adding a column/row for 'error in x'
  NCOL <- ncol(VCOV)
  ADD1 <- c(rep(0, NCOL))
  ADD1 <- matrix(ADD1, ncol = 1)
  colnames(ADD1) <- predNAME[1]
  VCOV <- cbind(VCOV, ADD1)
  ADD2 <- c(rep(0, NCOL + 1))
  ADD2 <- matrix(ADD2, nrow = 1)
  rownames(ADD2) <- predNAME[1]
  VCOV <- rbind(VCOV, ADD2) 
         
  # iterate over all entries in 'newdata' as in usual 'predict.' functions
  # NR is number of its, varPLACE index in VCOV for exp var
  NR <- nrow(newdata)
  varPLACE <- ncol(VCOV)   
   
  # define counter function
  counter <- function (i) 
  {
    if (i%%10 == 0) 
      cat(i)
    else cat(".")
    flush.console()
  }
   
  outMAT <- matrix(nrow = NR, ncol = 7) 
   
  for (i in 1:NR) {
    
    if(trace) counter(i)
     
    # get predictor values and optional errors
    predVAL <- newdata[i, 1]
    if (ncol(newdata) == 2){ 
      predERROR <- newdata[i, 2] 
    } else {
      predERROR <- 0
    }
    names(predVAL) <- predNAME[1] 
    names(predERROR) <- predNAME[1] 
     
    # create mean vector for 'mvrnorm'
    # these are expected values for coefficients and input value
    MU <- c(COEF, predVAL)
     
    # create variance-covariance matrix for 'mvrnorm'
    # by putting error^2 in lower-right position of VCOV
    newVCOV <- VCOV
    newVCOV[varPLACE, varPLACE] <- predERROR^2
     
    # create MC simulation matrix
    simMAT <- MASS::mvrnorm(n = nsim, mu = MU, Sigma = newVCOV, empirical = TRUE)
     
    # evaluate expression on rows of simMAT
    EVAL <- eval(expression(SSlogis(Depth, Asym, xmid, scal)), 
      envir = as.data.frame(simMAT))

    # collect statistics
    PRED <- data.frame(predVAL)
    colnames(PRED) <- predNAME[1]  
    FITTED <- predict(mod, newdata = data.frame(PRED))
    MEAN.sim <- mean(EVAL, na.rm = TRUE)
    SD.sim <- sd(EVAL, na.rm = TRUE)
    MEDIAN.sim <- median(EVAL, na.rm = TRUE)
    MAD.sim <- mad(EVAL, na.rm = TRUE)
    QUANT <- quantile(EVAL, c((1 - level)/2, level + (1 - level)/2))
    RES <- c(FITTED, MEAN.sim, SD.sim, MEDIAN.sim, MAD.sim, QUANT[1], QUANT[2])
    outMAT[i, ] <- RES
    
  }
   
  outMAT <- data.frame(outMAT)
  colnames(outMAT) <- c('fit', 'mean', 'sd', 'median', 'mad', 
    names(QUANT[1]), names(QUANT[2]))
  rownames(outMAT) <- NULL
   
  if(trace) cat("\n")
  
  ## use logis_est and get_ests for outMAT
  # returns lower and upper bounds on doc estimates
  
  # lower limits
  pred_dat <- data.frame(
    Depth = attr(doc_in, 'preds')$Depth, 
    sg_prp = outMAT[, '2.5%']
  )
  lower_est <- logis_est(pred_dat)
  lower_est <- try({get_ests(lower_est$pred, lower_est$asym)})
  if('try-error' %in% class(lower_est)) lower_est <- NA
  
  # upper limits
  pred_dat <- data.frame(
    Depth = attr(doc_in, 'preds')$Depth, 
    sg_prp = outMAT[, '97.5%']
  )
  upper_est <- logis_est(pred_dat)
  upper_est <- try({get_ests(upper_est$pred, upper_est$asym)})
  if('try-error' %in% class(upper_est)) upper_est <- NA
  
  # output
  # fill lower_est, upper_est attributes
  attr(doc_in, 'lower_est') <- lower_est
  attr(doc_in, 'upper_est') <- upper_est
  return(doc_in)
  
}
