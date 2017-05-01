#===========================================
# Regression Models Course : MTcars Analysis
#===========================================

# -----------
# Key Columns
# -----------
# [, 1]	 mpg	 Miles/(US) gallon
# [, 2]	 cyl	 Number of cylinders
# [, 3]  disp  Displacement (Cubic inches)
# [, 4]	 hp	 Gross horsepower
# [, 6]	 wt  Weight (1000 lbs)
# [, 9]	 am	 Transmission (0 = automatic, 1 = manual)
# [,10]  gear  Number of forward gears
# [,11]  carb  Number of carburetors

# Load data set
library(datasets)
library(ggplot2)
data("mtcars")

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

## --------------------------------------
## Fitting Simple Linear Regression (SLR)
## --------------------------------------

# So, we estimate a 7.25 mpg increase
# in moving from a automatic to a manual
# transmission.  We expect to get 17 mpg 
# with an automatic transmission.
fit1 <- lm(mpg ~ factor(am) -1, data = mtcars)
coef(fit1)

# This is consistent with the mean values:
# Automatic transmission:
mean(mtcars[mtcars[,9] == 0,1])

# Manual transmission:
mean(mtcars[mtcars[,9] == 1,1])

# Summarize SLR comparison of auto vs man 
# transmission
summary(fit1)$coef

# So we can reject the null hypothesis that the transmission 
# has no impact on MPG.

boxplot(mpg ~ am, data = mtcars,
        boxwex = 0.4, at = 0:1 - 0.2,
        main = "Auto vs Manual Transmission\n Simple Linear Regression (SLR)",
        col = "yellow",
        xlab = "Transmission : Auto = 0, Manual = 1",
        ylab = "Miles Per Gallon",
        xlim = c(-0.5,1), ylim = c(0,35), yaxs = "i")

## ---------------------------------
## Fitting Multivariable Models
## ---------------------------------

# And visually
# g <- ggplot(mtcars,aes(x=am,y=mpg))
# g <- g + geom_point()
# g

# Examine the correlation between MPG and some of the
# regressors likely to impact it.
regressor <- c("cyl","disp","hp","wt","am", "gear","carb")
correlate <- c(round(cor(mtcars$mpg,mtcars$cyl),3),
               round(cor(mtcars$mpg,mtcars$disp),3),
               round(cor(mtcars$mpg,mtcars$hp),3),
               round(cor(mtcars$mpg,mtcars$wt),3),
               round(cor(mtcars$mpg,mtcars$am),3),
               round(cor(mtcars$mpg,mtcars$gear),3),
               round(cor(mtcars$mpg,mtcars$carb),3)
               )

# Create data frame
mtCor <- data.frame(regressor, correlate)
# Display table of correlations
t(mtCor)

# Examine correlation between displacement
# and weight.  
cor(mtcars$disp,mtcars$wt)

# Examine correlation between cylinders
# and horse power.  
cor(mtcars$cyl,mtcars$hp)

# Update our model to include weight, cylinders,
# and horse power.
fit2 <- update(fit1, mpg ~ factor(am) + wt - 1)
fit3 <- update(fit1, mpg ~ factor(am) + wt + cyl - 1)
fit4 <- update(fit1, mpg ~ factor(am) + wt + cyl + hp - 1)

# Examine differences between fit3 and fit4 to 
# assess value of including horsepower as a variable.
summary(fit3)
summary(fit4)

# coef(fit4)

## -----------------------------
## Residual Plot and Diagnostics
## -----------------------------
library(car)

g <- ggplot(data.frame(x = mtcars$am, y = resid(fit1)), aes(x=x, y=y))
g <- g + ggtitle("Residual Plot (SLR)") 
g <- g + xlab("Transmission\n Auto=0, Man=1")
g <- g + ylab("Residuals")
g <- g + geom_point(size = 3, colour = "black")
g <- g + geom_hline(yintercept = 0, size = 2)
g <- g + scale_x_continuous(breaks = 0:1, limits = c(-0.5,1.5))
g

# Compute analysis of variance table  
mtAnova <- anova(fit1,fit2,fit3,fit4)
mtAnova

# Examine variance inflation with 3 factors
vif(lm(mpg ~ factor(am) + wt + cyl, mtcars))
sqrt(vif(lm(mpg ~ factor(am) + wt + cyl, mtcars)))

# Examine dfbetas to identify outliers
dfb <- round(dfbetas(fit3),3)
dfb[dfb[,1] > 0.5 | dfb[,1] < -0.5,]








