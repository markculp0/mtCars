# Regression Models Course : MTcars Analysis


# [, 1]	 mpg	 Miles/(US) gallon
# [, 2]	 cyl	 Number of cylinders
# [, 4]	 hp	 Gross horsepower
# [, 6]	 wt  Weight (1000 lbs)
# [, 9]	 am	 Transmission (0 = automatic, 1 = manual)

# Load data set
library(datasets)
library(ggplot2)
data("mtcars")

# Subset manual transmissions
mtcarMan <- mtcars[mtcars[,9] == 1,]
# Automatic transmissions
mtcarAuto <- mtcars[mtcars[,9] == 0,]

## --------------------
## Exploratory Analysis
## --------------------

# Examine columns and data types
str(mtcars)

# Range of MPG values
range(mtcars$mpg)

# Car transmissions: 
# 0 = automatic 
# 1 = manual 
table(mtcars$am)

## ---------------------------------
## Fitting Multiple Models, Strategy
## ---------------------------------

# So, we estimate a 7.25 mpg increase
# in moving from a automatic to a manual
# transmission.  We expect to get 17 mpg 
# with an automatic transmission.
fit1 <- lm(mpg ~ am, data = mtcars)
coef(fit1)

# This consistent with the mean values:
# Automatic transmission:
mean(mtcarAuto[,1])

# Manual transmission:
mean(mtcarMan[,1])

# Summarize SLR comparison of auto vs man 
# transmission
summary(lm(mpg ~ factor(am) - 1 , mtcars))

# And visually
g <- ggplot(mtcars,aes(x=am,y=mpg))
g <- g + geom_point()
g

## ---------------------------------


# Examine 
summary(lm(mpg ~ cyl + hp + wt + factor(am) - 1, mtcars))


## -----------------------------
## Residual Plot and Diagnostics
## -----------------------------


## ------------------------------------
## Conclusions : Quantified Uncertainty
## ------------------------------------








