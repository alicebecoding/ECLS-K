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



#######################################################################################################
############ CHECKING THE IMPUTATIONS #################################################################
#######################################################################################################

fit <- with(imp, lm(X4MSCALK1 ~ T1MDIFF))
summary(pool(fit))

# densityplot(imp0517, scales = list(x = list(relation = "free")), layout = c(5, 2))
# error: cannot allocate vector of size 40.0 Gb

# Density plots of original vs imputed data for specific variables
densityplot(imp0517, data = ~X1MSCALK1, scales = list(x = list(relation = "free")), layout = c(1,1))
densityplot(imp0517, data = ~X4MSCALK1, scales = list(x = list(relation = "free")), layout = c(1,1))
densityplot(imp0517, data = ~T1MDIFF, scales = list(x = list(relation = "free")), layout = c(1,1))
densityplot(imp0517, data = ~T1MARSAV, scales = list(x = list(relation = "free")), layout = c(1,1))
densityplot(imp0517, data = ~T4MARSAV, scales = list(x = list(relation = "free")), layout = c(1,1))

# Plot missingness against values for T1MDIFF
tdiff.na <- is.na(data0517$T1MDIFF)
fit.tdiff <- with(imp0517, glm(tdiff.na ~ X1MSCALK1, family = binomial)) # + X4MSCALK1 + X1KAGE_R
ps <- rep(rowMeans(sapply(fit.tdiff$analyses, fitted.values)), 41)
probMissing <- xyplot(imp0517, T1MDIFF ~ ps | .imp, pch = c(1,20), alpha = 0.1, 
       xlab = "Probability that T1 Math Diff Score is missing", 
       ylab = "T1 Math Diff Score", scales = list(tick.number = 3))
print(probMissing)

# ?panel.xyplot
# ?xyplot
# trellis.par.get()
# install.packages("hexbin",dependencies=T)
# library(hexbin)



#######################################################################################################
############ EXPLORING SOME SIMPLE RELATIONSHIPS ######################################################
#######################################################################################################

# Simple linear regressions: What predicts DA scores best at X2 and X4?
# T1MARS
ggplot(completeimp0519, aes(x=T1MARSAV, y=X2MSCALK1)) +
  geom_point(alpha=0.2) + stat_smooth(method="lm", se=F, color="black") +
  coord_cartesian(xlim=c(1,5), ylim=c(6,96)) +
  labs(x="Time 1 Teacher Rating", y="Time 2 Direct Assessment Score", 
       title="T2 DA Score predicted by T1 Teacher Rating") 

fitT1X2 <- lm(X2MSCALK1~T1MARSAV, data=completeimp0519)
summary(fitT1X2)
lm.beta(fitT1X2)

ggplot(completeimp0519, aes(x=T1MARSAV, y=X4MSCALK1)) +
  geom_point(alpha=0.2) + stat_smooth(method="lm", se=F, color="black") +
  coord_cartesian(xlim=c(1,5), ylim=c(6,96)) +
  labs(x="Time 1 Teacher Rating", y="Time 4 Direct Assessment Score", 
       title="T4 DA Score predicted by T1 Teacher Rating") 

fitT1X4 <- lm(X4MSCALK1~T1MARSAV, data=completeimp0519)
summary(fitT1X4)
lm.beta(fitT1X4)

#X1MSCAL
ggplot(completeimp0519, aes(x=X1MSCALK1, y=X2MSCALK1)) +
  geom_point(alpha=0.2) + stat_smooth(method="lm", se=F, color="black") +
  coord_cartesian(xlim=c(6,80), ylim=c(6,96)) +
  labs(x="Time 1 Direct Assessment Score", y="Time 2 Direct Assessment Score", 
       title="T2 DA Score predicted by T1 DA Score") 

fitX1X2 <- lm(X2MSCALK1~X1MSCALK1, data=completeimp0519)
summary(fitX1X2)
lm.beta(fitX1X2)

ggplot(completeimp0519, aes(x=X1MSCALK1, y=X4MSCALK1)) +
  geom_point(alpha=0.2) + stat_smooth(method="lm", se=F, color="black") +
  coord_cartesian(xlim=c(6,80), ylim=c(6,96)) +
  labs(x="Time 1 Direct Assessment Score", y="Time 4 Direct Assessment Score", 
       title="T4 DA Score predicted by T1 DA Score") 

fitX1X4 <- lm(X4MSCALK1~X1MSCALK1, data=completeimp0519)
summary(fitX1X4)
lm.beta(fitX1X4)

#T1DIFF
ggplot(completeimp0519, aes(x=T1MDIFF, y=X2MSCALK1)) +
  geom_point(alpha=0.2) + stat_smooth(method="lm", se=F, color="black") +
  coord_cartesian(xlim=c(-3.5,5), ylim=c(6,96)) +
  labs(x="Time 1 Difference Score", y="Time 2 Direct Assessment Score", 
       title="T2 DA Score predicted by T1 Difference Score", 
       caption="Difference Score calculated by subtracting standardized Direct Assessment Score from standarized Teacher Rating") 

fitT1DX2 <- lm(X2MSCALK1~X1MSCALK1 + T1MDIFF, data=completeimp0519)
summary(fitT1DX2)
lm.beta(fitT1DX2)

ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1)) +
  geom_point(alpha=0.2) + stat_smooth(method="lm", se=F, color="black") +
  coord_cartesian(xlim=c(-3.5,5), ylim=c(6,96)) +
  labs(x="Time 1 Difference Score", y="Time 4 Direct Assessment Score", 
       title="T4 DA Score predicted by T1 Difference Score", 
       caption="Difference Score calculated by subtracting standardized Direct Assessment Score from standarized Teacher Rating")

fitT1DX4 <- lm(X4MSCALK1~X1MSCALK1 + T1MDIFF, data=completeimp0519)
summary(fitT1DX4)
lm.beta(fitT1DX4)



#######################################################################################################
############ EXPLORING SOME SIMPLE RELATIONSHIPS ######################################################
#######################################################################################################

# Simple linear regressions: What predicts DA scores best at X2 and X4?
# T1MARS
fitT1X2 <- lm(X2MSCALK1~T1MARSAV, data=completeimp0519)
summary(fitT1X2)
lm.beta(fitT1X2)

fitT1X4 <- lm(X4MSCALK1~T1MARSAV, data=completeimp0519)
summary(fitT1X4)
lm.beta(fitT1X4)

#X1MSCAL
fitX1X2 <- lm(X2MSCALK1~X1MSCALK1, data=completeimp0519)
summary(fitX1X2)
lm.beta(fitX1X2)

fitX1X4 <- lm(X4MSCALK1~X1MSCALK1, data=completeimp0519)
summary(fitX1X4)
lm.beta(fitX1X4)

#T1DIFF
fitT1DX2 <- lm(X2MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=completeimp0519)
summary(fitT1DX2)
lm.beta(fitT1DX2)

fitT1DX4 <- lm(X4MSCALK1~X1MSCALK1 +  T1MDIFF, data=completeimp0519)
summary(fitT1DX4)
lm.beta(fitT1DX4)



# Breaking these down by gender

boysimp <- subset(completeimp0519, X_CHSEX_R==0)
girlsimp <- subset(completeimp0519, X_CHSEX_R==1)

fitT1X2 <- lm(X2MSCALK1~X1MSCALK1, data=boysimp)
summary(fitT1X2)
lm.beta(fitT1X2)

fitT1X2 <- lm(X2MSCALK1~X1MSCALK1, data=girlsimp)
summary(fitT1X2)
lm.beta(fitT1X2)

fitT1X4B <- lm(X4MSCALK1~X1MSCALK1, data=boysimp)
summary(fitT1X4)
lm.beta(fitT1X4)

fitT1X4G <- lm(X4MSCALK1~X1MSCALK1, data=girlsimp)
summary(fitT1X4)
lm.beta(fitT1X4)

ggplot(boysimp, aes(x=X1TCHCON, y=X2MSCALK1)) +
  geom_point() + stat_smooth(method="lm")
ggplot(boysimp, aes(x=X1TCHEXT, y=X2MSCALK1)) +
  geom_point() + stat_smooth(method="lm")


#######################################################################################################
############ ADDING REGRESSION INFO TO PLOT ###########################################################
#######################################################################################################


ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title=paste(fit$call$data, fit$call$formula[2], " ~ ", fit$call$formula[3])) +
    labs(caption = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}



#######################################################################################################
############ THE ACTUAL DATA ANALYSIS! ################################################################
#######################################################################################################


# Run the linear model!!!
fit <- with(imp, lm(X1MSCALK1 ~ X1KAGE_R))
round(summary(pool(fit)),5)

cor(may_4_new$X1MSCALK1,may_4_new$T1MARSAV, use='pairwise.complete.obs')
rcorr()

completeimp0519 <- complete(imp0517)

d10 <- lm(X4MSCALK1 ~ T1MDIFF, X1M_decile=="10", data=completeimp0519)
d9 <- lm(X4MSCALK1 ~ T1MDIFF, X1M_decile=="9", data=completeimp0519)
d8 <- lm(X4MSCALK1 ~ T1MDIFF, X1M_decile=="8", data=completeimp0519)
d7 <- lm(X4MSCALK1 ~ T1MDIFF, X1M_decile=="7", data=completeimp0519)
d6 <- lm(X4MSCALK1 ~ T1MDIFF, X1M_decile=="6", data=completeimp0519)
d5 <- lm(X4MSCALK1 ~ T1MDIFF, X1M_decile=="5", data=completeimp0519)
d4 <- lm(X4MSCALK1 ~ T1MDIFF, X1M_decile=="4", data=completeimp0519)
d3 <- lm(X4MSCALK1 ~ T1MDIFF, X1M_decile=="3", data=completeimp0519)
d2 <- lm(X4MSCALK1 ~ T1MDIFF, X1M_decile=="2", data=completeimp0519)
d1 <- lm(X4MSCALK1 ~ T1MDIFF, X1M_decile=="1", data=completeimp0519)

summary(d10)
summary(d9)
summary(d8)
summary(d7)
summary(d6)
summary(d5)
summary(d4)
summary(d3)
summary(d2)
summary(d1)


