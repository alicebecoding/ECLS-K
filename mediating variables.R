library(psych)
library(QuantPsyc)
library(ggplot2)
library(graphics)
library(lattice)
library(Hmisc)
library(dplyr)
library(tidyr)
require(mice)
# library(Rcmdr)

# 0 is male, 1 is female

# 1=White, non-hispanic
# 2=Black, non-hispanic
# 3=Hispanic, Race Specified
# 4=Hispanic, No Race Specified
# 5=Asian, non-hispanic
# 6=Native Hawaiian/Pacific Islander, non-hispanic
# 7=American Indian/Alaska Native, non-hispanic
# 8=Two or more races, non-hispanic

# Color-Blind Palette: 
# c(#CC79A7", "#D55E00", "#E69F00", "#F0E442", "#009E73", "#56B4E9", "#0072B2", "#999999")

# Mediator analysis
# http://rstudio-pubs-static.s3.amazonaws.com/27957_84bb7c62c25d430c89618d4d76b671b6.html

# In a 3-step mediator analysis, we do seperate regressions on the following equations to get the 
# regression coefficients. 

# Independent variable: X1MSCALK1
# Dependent variable: X4MSCALK1
# Mediator: X_CHSEX_R

# step1: lm(Y~X) regression coefficient for X should be significant
# Y = cX+d1+e1
fit1 <- lm(completeimp0519$X4MSCALK1~completeimp0519$X1MSCALK1)
summary(fit1) # significant

# step2: lm(M~X) regression coefficient for X should be significant
# M = aX + d2 + e2

# first need to dummy-code gender: 
completeimp0519$dX_CHSEX_R <- as.numeric(completeimp0519$X_CHSEX_R)

fit2 <- lm(completeimp0519$dX_CHSEX_R~completeimp0519$X1MSCALK1)
summary(fit2)

# step3: lm(Y ~X+M) regression coefficient for M should be significant
# Y = c'X + bM + d3 + e3
fit3 <- lm(completeimp0519$X4MSCALK1 ~ completeimp0519$dX_CHSEX_R + completeimp0519$X1MSCALK1)
summary(fit3) # both M and X significant

# CI for a*b: 
a = summary(fit2)$coefficients[2,1]
se.a = summary(fit2)$coefficients[2,2]
b = summary(fit3)$coefficients[2,1]
se.b = summary(fit3)$coefficients[2,2]
install.packages("RMediation")
library(RMediation)
medci(mu.x=a, mu.y=b, se.x=se.a, se.y=se.b, rho=0, alpha=.05,
      type="prodclin", plot=TRUE, plotCI=TRUE)
# It looks like this mediator is not significant. 
