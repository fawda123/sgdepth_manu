######
# new binning method

library(ggplot2)
library(dplyr)

data(shps)

# segment and seagrass
seg <- shps[['seg_820.shp']] # 820 segment polygon
sgrass <- shps[['sgpts_2006_820.shp']] # 820 segment seagrass depth point

# create sampling grid and test point
set.seed(4321)
est_pts <- grid_est(seg, spacing = 0.02)
test_pt <- est_pts[27, ]

# subset seagrass with buffer around test point
buff_pts <- buff_ext(sgrass, test_pt, buff = 0.02)

# create doc object with samples buffer points
dat_in <- data.frame(buff_pts)

doc_est(dat_in)



  
