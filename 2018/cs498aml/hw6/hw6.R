# import libraries
library(MASS)

# fetch data
housing <- read.table('hw6/housing.data', header=FALSE)

# (a) Regress house price (variable 14) against all others, and use leverage,
# Cook’s distance, and standardized residuals to find possible outliers. Produce
# a diagnostic plot that allows you to identify possible outliers.

# lm between column_idx 14 and all other columns from data housing
result_a <- lm(V14~., data = housing) # 365, 369, 373
plot(result_a, which=5)

# (b) Remove all the points you suspect as outliers, and compute a new regression.
# Produce a diagnostic plot that allows you to identify possible
# outliers.

# remove outliners from previous ploting
housing_b = housing[-c(365, 369, 373), ] # 366, 370, 368
result_b <- lm(V14~., data = housing_b)
plot(result_b, which=5)

# (c) Apply a Box-Cox transformation to the dependent variable – what is the
# best value of the parameter?

# using price(V14) as the dependent variable
bc <- boxcox(V14~., data = housing_b)
lambda <- bc$x[which.max(bc$y)]

# (d) Now transform the dependent variable, build a linear regression, and check
# the standardized residuals. If they look acceptable, produce a plot of fitted
# house price against true house price.
lm_fit <- lm(((V14 ^ lambda - 1)/lambda)~., data = housing_b)
plot(lm_fit, which=5)

# standard error VS. fitted values
std = rstandard(result_a)
plot(std ~ result_a$fitted.values)

pred = (lambda * lm_fit$fitted.values + 1) ^ ( 1 / lambda)
plot(pred ~ as.matrix(housing_b[,14]))
