
### Light requirements changes from 1988 to 2014



#### depth of colonization


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
  select(lon_lat, yr, Longitude, Latitude, z_c_all) %>% 
  filter(yr %in% c(1988, 2014)) %>% 
  spread(yr, z_c_all) %>% 
  mutate(zcdif = `2014` - `1988`)

range(tocmp$`1988`)
```

```
## [1] 0.7755338 1.2216710
```

```r
range(tocmp$`2014`)
```

```
## [1] 1.054655 1.439785
```

```r
mod <- lm(`2014` ~ `1988`, tocmp)
summary(mod)
```

```
## 
## Call:
## lm(formula = `2014` ~ `1988`, data = tocmp)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.09124 -0.05863 -0.01906  0.03841  0.17439 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept)   0.4799     0.2008   2.389  0.02954 * 
## `1988`        0.6875     0.1817   3.783  0.00163 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.07702 on 16 degrees of freedom
## Multiple R-squared:  0.4721,	Adjusted R-squared:  0.4391 
## F-statistic: 14.31 on 1 and 16 DF,  p-value: 0.001631
```

```r
tocmp <- mutate(tocmp, 
  res = resid(mod)
)

summary(lm(res ~ Latitude*Longitude, tocmp))
```

```
## 
## Call:
## lm(formula = res ~ Latitude * Longitude, data = tocmp)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.11669 -0.02643 -0.00413  0.02045  0.12330 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)   
## (Intercept)        19450.131   6017.970   3.232  0.00602 **
## Latitude            -702.523    216.939  -3.238  0.00595 **
## Longitude            235.516     72.857   3.233  0.00602 **
## Latitude:Longitude    -8.507      2.626  -3.239  0.00594 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.05859 on 14 degrees of freedom
## Multiple R-squared:  0.4937,	Adjusted R-squared:  0.3853 
## F-statistic: 4.551 on 3 and 14 DF,  p-value: 0.01996
```

```r
# light requirements between years
ggplot(tocmp, aes(x = `1988`, y = `2014`)) + 
  geom_point() + 
  stat_smooth(method = 'lm') + 
  theme_bw() + 
  geom_abline(intercept = 0, slope = 1) + 
  coord_cartesian(ylim = c(0.5, 1.5), xlim = c(0.5, 1.5)) +
  ggtitle('Zcmed difference')
```

![](tampa_comp2_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
# residual variation by location of difference in light requirements
ggplot(tocmp, aes(x = Longitude, y = Latitude, colour = zcdif, size = zcdif)) + 
  geom_point() + 
  coord_equal() + 
  theme_bw()
```

![](tampa_comp2_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
# t.test of difference
with(tocmp, t.test(zcdif))
```

```
## 
## 	One Sample t-test
## 
## data:  zcdif
## t = 7.0937, df = 17, p-value = 1.802e-06
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  0.09554008 0.17642970
## sample estimates:
## mean of x 
## 0.1359849
```

```r
# intercept only model of SI difference
mod1 <- gls(zcdif ~ 1,
  data = tocmp)
summary(mod1)
```

```
## Generalized least squares fit by REML
##   Model: zcdif ~ 1 
##   Data: tocmp 
##         AIC       BIC  logLik
##   -30.17959 -28.51316 17.0898
## 
## Coefficients:
##                 Value  Std.Error  t-value p-value
## (Intercept) 0.1359849 0.01916983 7.093692       0
## 
## Standardized residuals:
##         Min          Q1         Med          Q3         Max 
## -1.24918048 -0.86964815  0.01210872  0.36105353  1.98295530 
## 
## Residual standard error: 0.08133071 
## Degrees of freedom: 18 total; 17 residual
```

```r
# intercept only model of SI difference, lat/lon correlation structure
mod2 <- gls(zcdif ~ 1,
  correlation = corGaus(form = ~ Latitude + Longitude, nugget = TRUE), 
  data = tocmp)
summary(mod2)
```

```
## Generalized least squares fit by REML
##   Model: zcdif ~ 1 
##   Data: tocmp 
##         AIC       BIC   logLik
##   -40.40087 -37.06801 24.20043
## 
## Correlation Structure: Gaussian spatial correlation
##  Formula: ~Latitude + Longitude 
##  Parameter estimate(s):
##      range     nugget 
## 0.10688151 0.04121191 
## 
## Coefficients:
##                 Value Std.Error  t-value p-value
## (Intercept) 0.1614768 0.0538006 3.001393   0.008
## 
## Standardized residuals:
##         Min          Q1         Med          Q3         Max 
## -1.17066995 -0.88633435 -0.22574537  0.03567487  1.25076086 
## 
## Residual standard error: 0.1085606 
## Degrees of freedom: 18 total; 17 residual
```

```r
AIC(mod1, mod2)
```

```
##      df       AIC
## mod1  2 -30.17959
## mod2  4 -40.40087
```

```r
anova(mod1, mod2)
```

```
##      Model df       AIC       BIC   logLik   Test  L.Ratio p-value
## mod1     1  2 -30.17959 -28.51316 17.08979                        
## mod2     2  4 -40.40087 -37.06801 24.20043 1 vs 2 14.22128   8e-04
```

#### light requirements


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

range(tocmp$`1988`)
```

```
## [1] 20.71158 60.09769
```

```r
range(tocmp$`2014`)
```

```
## [1] 33.70088 63.16836
```

```r
mod <- lm(`2014` ~ `1988`, tocmp)
summary(mod)
```

```
## 
## Call:
## lm(formula = `2014` ~ `1988`, data = tocmp)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -7.049 -2.060  0.380  3.304  5.571 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 18.25800    4.42396   4.127  0.00079 ***
## `1988`       0.70820    0.09603   7.375 1.57e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.029 on 16 degrees of freedom
## Multiple R-squared:  0.7727,	Adjusted R-squared:  0.7585 
## F-statistic: 54.38 on 1 and 16 DF,  p-value: 1.568e-06
```

```r
tocmp <- mutate(tocmp, 
  res = resid(mod)
)

summary(lm(res ~ Latitude*Longitude, tocmp))
```

```
## 
## Call:
## lm(formula = res ~ Latitude * Longitude, data = tocmp)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.406 -1.823 -0.284  1.296  5.371 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)        -455604.9   320466.4  -1.422    0.177
## Latitude             16543.3    11552.3   1.432    0.174
## Longitude            -5525.5     3879.7  -1.424    0.176
## Latitude:Longitude     200.6      139.9   1.435    0.173
## 
## Residual standard error: 3.12 on 14 degrees of freedom
## Multiple R-squared:  0.4753,	Adjusted R-squared:  0.3629 
## F-statistic: 4.227 on 3 and 14 DF,  p-value: 0.02527
```

```r
# light requirements between years
ggplot(tocmp, aes(x = `1988`, y = `2014`)) + 
  geom_point() + 
  stat_smooth(method = 'lm') + 
  theme_bw() + 
  geom_abline(intercept = 0, slope = 1) + 
  coord_cartesian(ylim = c(0, 100), xlim = c(0, 100)) +
  ggtitle('SI difference')
```

![](tampa_comp2_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# residual variation by location of difference in light requirements
ggplot(tocmp, aes(x = Longitude, y = Latitude, colour = sidif, size = sidif)) + 
  geom_point() + 
  coord_equal() + 
  theme_bw()
```

![](tampa_comp2_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

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
AIC(mod1, mod2)
```

```
##      df      AIC
## mod1  2 109.2254
## mod2  4 103.5805
```

```r
anova(mod1, mod2)
```

```
##      Model df      AIC      BIC    logLik   Test  L.Ratio p-value
## mod1     1  2 109.2254 110.8919 -52.61272                        
## mod2     2  4 103.5805 106.9133 -47.79024 1 vs 2 9.644969   0.008
```
