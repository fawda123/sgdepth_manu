
### Light requirements changes from 1988 to 2014




```r
data(tb_light_allsec)

##
# formatting

# melt, rename yr
sec <- reshape2::melt(tb_light_allsec, id.vars = names(tb_light_allsec[[1]])) %>% 
  rename(yr = L1)

# complete yeear, locations
yrloc <- select(sec, yr, Longitude, Latitude) %>% 
  unite('lon_lat', Longitude, Latitude, remove = F) %>% 
  complete(lon_lat, yr) %>% 
  arrange(yr)

# locations that aren't common between all years
torm <- unique(yrloc$lon_lat[is.na(yrloc$Longitude)])

# remove unique locations, get 1988, 2014, take difference
tocmp <- unite(sec, 'lon_lat', Longitude, Latitude, remove = F) %>% 
  filter(!lon_lat %in% torm) %>% 
  rename(Segment = seg) %>% 
  select(lon_lat, yr, Longitude, Latitude, light) %>% 
  filter(yr %in% c(1988, 2014)) %>% 
  spread(yr, light) %>% 
  mutate(sidif = `2014` - `1988`)

# light requirements between years
ggplot(tocmp, aes(x = `1988`, y = `2014`)) + 
  geom_point() + 
  theme_bw() + 
  geom_abline(intercept = 0, slope = 1) + 
  coord_cartesian(ylim = c(0, 100), xlim = c(0, 100))
```

![](tampa_comp2_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
# residual variation by location of difference in light requirements
ggplot(tocmp, aes(x = Longitude, y = Latitude, colour = sidif, size = sidif)) + 
  geom_point() + 
  coord_equal() + 
  theme_bw()
```

![](tampa_comp2_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
# t.test of difference
with(tocmp, t.test(sidif))
```

```
## 
## 	One Sample t-test
## 
## data:  sidif
## t = 4.4335, df = 17, p-value = 0.0003641
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  2.688252 7.569878
## sample estimates:
## mean of x 
##  5.129065
```

```r
# intercept only model of SI difference
mod1 <- gls(sidif ~ 1,
  data = tocmp)
summary(mod1)
```

```
## Generalized least squares fit by REML
##   Model: sidif ~ 1 
##   Data: tocmp 
##        AIC      BIC    logLik
##   109.2254 110.8919 -52.61272
## 
## Coefficients:
##                Value Std.Error  t-value p-value
## (Intercept) 5.129065  1.156885 4.433515   4e-04
## 
## Standardized residuals:
##        Min         Q1        Med         Q3        Max 
## -1.7632801 -0.6761684 -0.1307283  0.5684561  1.7955017 
## 
## Residual standard error: 4.908246 
## Degrees of freedom: 18 total; 17 residual
```

```r
# intercept only model of SI difference, lat/lon correlation structure
mod2 <- gls(sidif ~ 1,
  correlation = corGaus(form = ~ Latitude + Longitude, nugget = TRUE), 
  data = tocmp)
summary(mod2)
```

```
## Generalized least squares fit by REML
##   Model: sidif ~ 1 
##   Data: tocmp 
##        AIC      BIC    logLik
##   103.5805 106.9133 -47.79024
## 
## Correlation Structure: Gaussian spatial correlation
##  Formula: ~Latitude + Longitude 
##  Parameter estimate(s):
##      range     nugget 
## 0.34850237 0.04341715 
## 
## Coefficients:
##                Value Std.Error   t-value p-value
## (Intercept) 5.608698  9.961507 0.5630371  0.5808
## 
## Standardized residuals:
##         Min          Q1         Med          Q3         Max 
## -0.68421693 -0.28452843 -0.08399143  0.17307133  0.62420800 
## 
## Residual standard error: 13.34993 
## Degrees of freedom: 18 total; 17 residual
```

```r
mod3 <- lm(sidif ~ Latitude + Longitude,
  data = tocmp)
summary(mod3)
```

```
## 
## Call:
## lm(formula = sidif ~ Latitude + Longitude, data = tocmp)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.2328 -2.8316 -0.1651  2.6852  6.1899 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 6539.504   1434.155   4.560 0.000376 ***
## Latitude     -23.304      9.876  -2.360 0.032260 *  
## Longitude     71.307     15.081   4.728 0.000269 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.282 on 15 degrees of freedom
## Multiple R-squared:  0.6054,	Adjusted R-squared:  0.5528 
## F-statistic: 11.51 on 2 and 15 DF,  p-value: 0.0009356
```

```r
AIC(mod1, mod2, mod3)
```

```
## Warning in AIC.default(mod1, mod2, mod3): models are not all fitted to the
## same number of observations
```

```
##      df       AIC
## mod1  2 109.22545
## mod2  4 103.58048
## mod3  4  98.58751
```

```r
anova(mod1, mod2, mod3)
```

```
## Warning in nlme::anova.lme(object = mod1, mod2, mod3): fitted objects with
## different fixed effects. REML comparisons are not meaningful.
```

```
##      Model df       AIC       BIC    logLik   Test  L.Ratio p-value
## mod1     1  2 109.22545 110.89187 -52.61272                        
## mod2     2  4 103.58048 106.91333 -47.79024 1 vs 2 9.644969   0.008
## mod3     3  4  84.37155  87.20375 -38.18577
```
