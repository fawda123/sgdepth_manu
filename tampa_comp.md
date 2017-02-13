
### Annual changes in depth of colonization and light requirements in Tampa Bay



Secchi data from the Tampa Bay Water Atlas ([link](http://www.tampabay.wateratlas.usf.edu/)) were obtained for all locations in the same years with seagrass coverage: 1988, 1990, 1992, 1994, 1996, 1999, 2001, 2004, 2006, 2008, and 2010.  Secchi data with a minimum of ten monthly observations were averaged within each year at each location. 

Satellite data were available for 2003 through 2010, with four years overlapping with seagrass data.


```r
# load data
data(tb_seg)
data(tb_sats)
sats_melt <- reshape2::melt(tb_sats[['sats_all']], id.var = c('lat', 'lon'))

# color ramp
cols <- rev(c('blue', 'lightblue', 'yellow', 'brown'))

# prep data
to_plo <- sats_melt
to_plo$variable <- factor(to_plo$variable, labels = c('Average', seq(2003, 2010)))
seg_plo <- fortify(tb_seg)

# leg lab
leg_lab <- 'Clarity (m)'

# make plot
p <- ggplot(data = seg_plo) +
  geom_polygon(aes(x = long, y = lat, group = group), 
    colour = 'black', fill = colors()[245]) +
  geom_tile(data = subset(to_plo, !is.na(value)), 
    aes(x = lon, y = lat, fill = value, colour = value)
  ) +
  geom_polygon(aes(x = long, y = lat, group = group), 
    colour = 'black', fill = NA) +
  coord_equal() +
  facet_wrap(~ variable) + 
  scale_colour_gradientn(leg_lab, colours = cols) +
  scale_fill_gradientn(leg_lab, colours = cols) +
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 8), 
    axis.text.y = element_text(size = 8), 
    legend.position = 'top'
    )
p
```

![](tampa_comp_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

Secchi and satellite clarity (m) by year, all matched locations:

```r
# polygon segment
data(tb_seg)

# secchi data
data(secc_all_tb)

# seagrass points
data(sgpts_all_tb)

# 1km seagrass buffer for clipping
data(sgbuff_2010_tb)

# satellite data, 2003 to 2010
data(tb_sats)

sat_dat <- tb_sats$sats_all
  
# yrs to eval, years with satellite data and seagrass coverage
# note that previous analysis averaged water clarity from sat data from 2006 to 2010
yrsinsat <- names(tb_sats$sats_all) %>% 
  grep('[0-9]+', ., value = T) %>% 
  gsub('^.*_', '', .)
yrsinsat <- yrsinsat[yrsinsat %in% names(secc_all_tb)]

out_ls <- vector('list', length = length(yrsinsat))
names(out_ls) <- yrsinsat

# process years
for(i in seq_along(yrsinsat)){
  
  source('R/funcs.R')
  
  # get yr index and data in the year
  yr <- yrsinsat[i]
  secc <- secc_all_tb[[yr]]

  # satellite data
  yrcol <- paste0('clarity_', yr)
  torast <- sat_dat[, c('lon', 'lat', yrcol)]
  sat_rast <- make_rast_fun(torast, yrcol)
  
  # extract secchi locations on sat data
  # sample the satellit clarity raster
  samp_vals <- raster::extract(sat_rast, secc, sp = T)
  samp_vals <- data.frame(samp_vals, ests = 'Satellite') %>% 
    .[, c('Longitude', 'Latitude', yrcol, 'ests')]
  names(samp_vals)[names(samp_vals) %in% yrcol] <- 'clarity'
  
  # format secchi data for combo with sat ests
  secc <- as.data.frame(secc) %>% 
    select(Longitude, Latitude, SD) %>% 
    rename(
      clarity = SD
      ) %>% 
    mutate(
      ests = 'Secchi'
      )
  
  out <- rbind(samp_vals, secc) %>% 
    data.frame(., yr= yr)
  
  out_ls[[i]] <- out

}

toplo <- do.call('rbind', out_ls) %>% 
  spread(ests, clarity)

ggplot(toplo, aes(x = Secchi, y = Satellite)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  facet_wrap(~yr) + 
  theme_bw()
```

```
## Warning: Removed 26 rows containing missing values (geom_point).
```

![](tampa_comp_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Regression models comparing Satellite, Secchi all dates (2006 is removed in the second). Chen et al. 2007 report R2 of 0.67 between in situ secchi and satellie-derived Kd. 

```r
modall <- lm(Satellite ~ Secchi, toplo)
modno06 <- lm(Satellite ~ Secchi, toplo[!toplo$yr %in% '2006', ])
summary(modall)
```

```
## 
## Call:
## lm(formula = Satellite ~ Secchi, data = toplo)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.87240 -0.35453 -0.02326  0.26654  1.18229 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.19859    0.11604  10.329  < 2e-16 ***
## Secchi       0.38984    0.04654   8.377 1.96e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.4156 on 110 degrees of freedom
##   (26 observations deleted due to missingness)
## Multiple R-squared:  0.3895,	Adjusted R-squared:  0.3839 
## F-statistic: 70.17 on 1 and 110 DF,  p-value: 1.963e-13
```

```r
summary(modno06)
```

```
## 
## Call:
## lm(formula = Satellite ~ Secchi, data = toplo[!toplo$yr %in% 
##     "2006", ])
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.8354 -0.2969  0.0026  0.2659  0.9698 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.77972    0.12985   6.005 4.72e-08 ***
## Secchi       0.63113    0.05759  10.958  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3666 on 84 degrees of freedom
##   (20 observations deleted due to missingness)
## Multiple R-squared:  0.5884,	Adjusted R-squared:  0.5835 
## F-statistic: 120.1 on 1 and 84 DF,  p-value: < 2.2e-16
```

Comparisons of satellite and secchi data, within 1km of seagrass:

```r
# secchi and satellite light ests, all years
data(tb_light_allsat)
data(tb_light_allsec)

sec <- reshape2::melt(tb_light_allsec, id.vars = names(tb_light_allsec[[1]])) %>% 
  mutate(est = 'Secchi')
sat <- reshape2::melt(tb_light_allsat, id.vars = names(tb_light_allsat[[1]])) %>% 
  mutate(est = 'Satellite')

# subset secchi data with estimable sat data
tb_ts <- rbind(sec, sat) %>%
  rename(yr = L1)

ggplot(tb_ts, aes(x = factor(yr), y = SD, fill = est)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~seg, ncol = 1)
```

![](tampa_comp_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
ggplot(tb_ts, aes(x = factor(yr), y = light, fill = seg)) +
  geom_boxplot() +
  theme_bw() + 
  facet_wrap(~est, ncol = 1)
```

![](tampa_comp_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
summs <- mutate(tb_ts, 
    Kd = 1.7/SD
  ) %>% 
  group_by(seg, yr, est) %>% 
  summarise(
    Kd = mean(Kd), 
    z_c_all = mean(z_c_all), 
    light = mean(light)
  )
labs <- filter(summs, yr %in% c('1988', '2004', '2010')) %>% 
  filter(!(yr %in% '2004' & est %in% 'Secchi'))
ggplot(summs, aes(x = Kd, y = z_c_all, colour = seg)) +
  geom_path() +
  geom_point() + 
  geom_label(data = labs, aes(label = yr), size = 3, label.padding = unit(0.1, "lines")) +
  theme_bw() + 
  facet_wrap(~est, ncol = 1)
```

![](tampa_comp_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
toplo1 <- filter(tb_ts, est %in% 'Secchi')
toplo2 <- filter(tb_ts, as.numeric(yr) > 2003)

ggplot(toplo1, aes(x = Longitude, y = Latitude, size = z_c_all, fill= z_c_all)) + 
  geom_point(pch = 21) + 
  facet_wrap( ~ yr) + 
  coord_equal() + 
  theme_bw() + 
  scale_fill_distiller(palette = 'Set1')
```

![](tampa_comp_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

```r
ggplot(toplo1, aes(x = Longitude, y = Latitude, size = light, fill= light)) + 
  geom_point(pch = 21) + 
  facet_wrap( ~ yr) + 
  coord_equal() + 
  theme_bw() + 
  scale_fill_distiller(palette = 'Set1')
```

![](tampa_comp_files/figure-html/unnamed-chunk-5-4.png)<!-- -->

```r
ggplot(toplo2, aes(x = Longitude, y = Latitude, size = light, fill= light)) + 
  geom_point(pch = 21) + 
  facet_grid(est ~ yr) + 
  coord_equal() + 
  theme_bw() + 
  scale_fill_distiller(palette = 'Set1')
```

![](tampa_comp_files/figure-html/unnamed-chunk-5-5.png)<!-- -->
