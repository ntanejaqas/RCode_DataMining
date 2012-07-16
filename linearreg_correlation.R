amazon <- read.table(file="C:\\Users\\Arvind\\Desktop\\amazon.txt",sep = " ", header = TRUE);
plot(amazon$year, amazon$high)
plot(amazon$year, amazon$low)
plot(amazon$low, amazon$high)
model1 <- lm(high~year, amazon); model2 <- lm(low~year, amazon); model3 <- lm(high~low, amazon);
coef(model1)
# (Intercept)         year 
# -330.2123529    0.1808824 
coef(model2)
# (Intercept)         year 
# 35.106960784 -0.007892157 
coef(model3)
# (Intercept)         low 
# 26.40087963 -0.01405959 

summary(model1)$r.squared; summary(model2)$r.squared; summary(model3)$r.squared;
# [1] 0.5816251
# [1] 0.0006995573
# [1] 0.0003128701

mean(residuals(model1)^2);mean(residuals(model2)^2);mean(residuals(model3)^2);
# [1] 0.5648408
# [1] 2.135381
# [1] 1.349661

> summary(model1)

Call:
lm(formula = high ~ year, data = amazon)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.3629 -0.5341  0.1479  0.4903  1.1412 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -330.21235   78.03319  -4.232 0.000725 ***
year           0.18088    0.03961   4.567 0.000371 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.8001 on 15 degrees of freedom
Multiple R-squared: 0.5816,	Adjusted R-squared: 0.5537 
F-statistic: 20.85 on 1 and 15 DF,  p-value: 0.0003708


> summary(model2)

Call:
lm(formula = low ~ year, data = amazon)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.1147 -0.7121 -0.1610  0.9306  2.9664 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)  35.106961 151.723912   0.231     0.82
year         -0.007892   0.077017  -0.102     0.92

Residual standard error: 1.556 on 15 degrees of freedom
Multiple R-squared: 0.0006996,	Adjusted R-squared: -0.06592 
F-statistic: 0.0105 on 1 and 15 DF,  p-value: 0.9197 

> summary(model3)

Call:
lm(formula = high ~ low, data = amazon)

Residuals:
     Min       1Q   Median       3Q      Max 
-2.05605 -0.87774  0.05615  1.01720  1.40344 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 26.40088    4.02478   6.560 9.05e-06 ***
low         -0.01406    0.20520  -0.069    0.946    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 1.237 on 15 degrees of freedom
Multiple R-squared: 0.0003129,	Adjusted R-squared: -0.06633 
F-statistic: 0.004695 on 1 and 15 DF,  p-value: 0.9463
