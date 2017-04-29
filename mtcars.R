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

# The fit is also consistent with mean for 
# manual transmission:
fit1$coefficients[1] + fit1$coefficients[2]

# Summarize SLR comparison of auto vs man 
# transmission
summary(lm(mpg ~ factor(am) - 1 , mtcars))

# And visually
# g <- ggplot(mtcars,aes(x=am,y=mpg))
# g <- g + geom_point()
# g

boxplot(mpg ~ am, data = mtcars,
        boxwex = 0.4, at = 0:1 - 0.2,
        main = "Auto vs Manual Transmission\n Simple Linear Regression (SLR)",
        col = "yellow",
        xlab = "Transmission : Auto = 0, Manual = 1",
        ylab = "Miles Per Gallon",
        xlim = c(-0.5,1), ylim = c(0,35), yaxs = "i")

## ---------------------------------
library(car)

# Examine fit with 4 variables 
fit4 <- lm(mpg ~ factor(am) + cyl + hp + wt - 1, mtcars)
summary(fit4)

# See how correlated the variables are
fit <- lm(mpg ~ ., mtcars)
vif(fit)
sqrt(vif(fit))

# See how these regressors correlate with MPG
cor(mtcars$disp,mtcars$wt)

regressor <- c("cyl","disp","hp","wt","am", "gear","carb")
correlate <- c(round(cor(mtcars$mpg,mtcars$cyl),3),
               round(cor(mtcars$mpg,mtcars$disp),3),
               round(cor(mtcars$mpg,mtcars$hp),3),
               round(cor(mtcars$mpg,mtcars$wt),3),
               round(cor(mtcars$mpg,mtcars$am),3),
               round(cor(mtcars$mpg,mtcars$gear),3),
               round(cor(mtcars$mpg,mtcars$carb),3)
               )

mtCor <- data.frame(regressor, correlate)
t(mtCor[,2])

## -----------------------------
## Residual Plot and Diagnostics
## -----------------------------


g <- ggplot(data.frame(x = mtcars$am, y = resid(fit1)), aes(x=x, y=y))
g <- g + ggtitle("Residual Plot (SLR)") 
g <- g + xlab("Transmission\n Auto=0, Man=1")
g <- g + ylab("Residuals")
g <- g + geom_point(size = 3, colour = "black")
g <- g + geom_hline(yintercept = 0, size = 2)
g <- g + scale_x_continuous(breaks = 0:1, limits = c(-0.5,1.5))
g
  
## ------------------------------------
## Conclusions : Quantified Uncertainty
## ------------------------------------








