# February 2017
# Scale scores not thetas

setwd("Z:/wgehring/Drobo/Alice/ECLS-K/R")
library(readr)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(readxl)
library(ggplot2)
library(mice)
library(ggm)
# library(Rcmdr)

# f1 is the whole data set from February, with the variables in my Research Notes
# f2 is the data set of complete cases that still includes 6s
# f3 is complete cases first, then 6s coded as NAs
# f3_test is f3 with z-scores
# f4 is with 6s coded as NAs, then complete cases
# f4_test is f4 with z-scores


feb_data <- read.csv("feb_data.csv", header = T)
View(feb_data)

# FIRST need to recode ALL -9s as NA
feb_data[feb_data==-9] <- NA


# Complete cases but still 6s
f_complete_with_6 <- feb_data[c(complete.cases(feb_data)),]

# check whole data.frame for NAs
apply(f2, 2, function(x) any(x==-9))
apply(f2, 2, function(x) any(x==NA))
# good


# Otherwise complete cases, but with 6s as NAs
f_complete_6_as_NA <- f_complete_with_6
f_complete_6_as_NA[f_complete_6_as_NA==6] <- NA


# Replace 6s with NAs then complete cases
a <- feb_data[,20:70]
a[a == 6] <- NA
b <- cbind(feb_data[,1:19], a)
f_complete_no_6 <- b[c(complete.cases(b)),]
View(f_complete_no_6)

# Rename data frames to shorter things

# f1 is the whole data set from February, with the variables in my Research Notes
# f2 is the data set of complete cases that still includes 6s
# f3 is complete cases first, then 6s coded as NAs
# f3_test is f3 with z-scores
# f4 is with 6s coded as NAs, then complete cases
# f4_test is f4 with z-scores


f1 <- feb_data
f2 <- f_complete_with_6
f3 <- f_complete_6_as_NA
f4 <- f_complete_no_6


# Create ARS averages
f3$T1RARS <- rowMeans(f3[,c("T1CMPSEN","T1STORY","T1LETTER","T1PRDCT","T1READS","T1USESTR","T1WRITE","T1CMPSTR","T1PRINT")],na.rm=T)
f3$T2RARS <- rowMeans(f3[,c("T2CMPSEN","T2STORY","T2LETTER","T2PRDCT","T2READS","T2USESTR","T2WRITE","T2CMPSTR","T2PRINT")],na.rm=T)
f3$T4RARS <- rowMeans(f3[,c("T4RLVINF","T4INTSTY","T4REGVWL","T4IRGVWL","T4RD1IND","T4RD1FLN","T4CMPSES","T4CONPRN","T4WRTSKIL")],na.rm=T)
f3$T1MARS <- rowMeans(f3[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH","T1MEASU","T1STRAT","T1FRACTN")],na.rm=T)
f3$T2MARS <- rowMeans(f3[,c("T2SORTS","T2ORDER","T2RELAT","T2SOLVE","T2GRAPH","T2MEASU","T2STRAT","T2FRACTN")],na.rm=T)
f3$T4MARS <- rowMeans(f3[,c("T4PLCVL","T4WHNUM","T4COINS","T4GRPHAC","T4ESTQNT","T4MEASU","T4STRAT","T4FRACTN")],na.rm=T)
View(f3)

# Convert scores to Z-scores
f3_test <- f3
f3_test$X1RSCALK1 <- scale(f3_test$X1RSCALK1, center = T, scale = T)
f3_test$X2RSCALK1 <- scale(f3_test$X2RSCALK1, center = T, scale = T)
f3_test$X4RSCALK1 <- scale(f3_test$X4RSCALK1, center = T, scale = T)
f3_test$X1MSCALK1 <- scale(f3_test$X1MSCALK1, center = T, scale = T)
f3_test$X2MSCALK1 <- scale(f3_test$X2MSCALK1, center = T, scale = T)
f3_test$X4MSCALK1 <- scale(f3_test$X4MSCALK1, center = T, scale = T)

f3_test$T1RARS <- scale(f3_test$T1RARS, center = T, scale = T)
f3_test$T2RARS <- scale(f3_test$T2RARS, center = T, scale = T)
f3_test$T4RARS <- scale(f3_test$T4RARS, center = T, scale = T)
f3_test$T1MARS <- scale(f3_test$T1MARS, center = T, scale = T)
f3_test$T2MARS <- scale(f3_test$T2MARS, center = T, scale = T)
f3_test$T4MARS <- scale(f3_test$T4MARS, center = T, scale = T)

# Remove ARS item scores
f3_test <- f3_test[,-c(20:70)]

# Calculate difference scores
# What these difference scores mean: 
# Positive values mean the teacher rated the student ABOVE their DA score
# Negative values mean the teacher rated the student BELOW their DA score
f3_test$T1RDIFF <- f3_test$T1RARS - f3_test$X1RSCALK1
f3_test$T2RDIFF <- f3_test$T2RARS - f3_test$X2RSCALK1
f3_test$T4RDIFF <- f3_test$T4RARS - f3_test$X4RSCALK1
f3_test$T1MDIFF <- f3_test$T1MARS - f3_test$X1MSCALK1
f3_test$T2MDIFF <- f3_test$T2MARS - f3_test$X2MSCALK1
f3_test$T4MDIFF <- f3_test$T4MARS - f3_test$X4MSCALK1

# Damn figure out some outliers
# Go here if outliers are a further issue: 
# https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/

hist(f3_test$X1RSCALK1)
hist(f3_test$X2RSCALK1)
hist(f3_test$X4RSCALK1)
hist(f3_test$X1MSCALK1)
hist(f3_test$X2MSCALK1)
hist(f3_test$X4MSCALK1)


# There is one outlier in X1MSCALK1 (Just assuming, didn't statistically test)
# SO I guess I should test
# ::crosses fingers:: ::pours out an offering of full-caf coffee::
source("http://goo.gl/UUyEzD")
outlierKD(f3, X1MSCALK1)
# IT WORKS MAHAHAHAHA
# This identifies way more outliers than just the one that's way off the map

# Gonna just replace it with NA for now
which(f3_test$X1MSCALK1 > 5)
f3_test[f3_test$X1MSCALK1 == 5.859013] <- NA
        # OR
f3_test[5606,17]<- NA
f3_test[5606,17]

# Okay let's look at difference scores
hist(f3_test$T1RDIFF)
hist(f3_test$T2RDIFF)
hist(f3_test$T4RDIFF)
hist(f3_test$T1MDIFF)
hist(f3_test$T2MDIFF)
hist(f3_test$T4MDIFF)

# Testing homogeneity of variances???
# Bartlett.test()
# leveneTest()

# f4

# Create ARS averages
f4$T1RARS <- rowMeans(f4[,c("T1CMPSEN","T1STORY","T1LETTER","T1PRDCT","T1READS","T1USESTR","T1WRITE","T1CMPSTR","T1PRINT")],na.rm=T)
f4$T2RARS <- rowMeans(f4[,c("T2CMPSEN","T2STORY","T2LETTER","T2PRDCT","T2READS","T2USESTR","T2WRITE","T2CMPSTR","T2PRINT")],na.rm=T)
f4$T4RARS <- rowMeans(f4[,c("T4RLVINF","T4INTSTY","T4REGVWL","T4IRGVWL","T4RD1IND","T4RD1FLN","T4CMPSES","T4CONPRN","T4WRTSKIL")],na.rm=T)
f4$T1MARS <- rowMeans(f4[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH","T1MEASU","T1STRAT","T1FRACTN")],na.rm=T)
f4$T2MARS <- rowMeans(f4[,c("T2SORTS","T2ORDER","T2RELAT","T2SOLVE","T2GRAPH","T2MEASU","T2STRAT","T2FRACTN")],na.rm=T)
f4$T4MARS <- rowMeans(f4[,c("T4PLCVL","T4WHNUM","T4COINS","T4GRPHAC","T4ESTQNT","T4MEASU","T4STRAT","T4FRACTN")],na.rm=T)
View(f4)

# Convert scores to Z-scores
f4_test <- f4
f4_test$X1RSCALK1 <- scale(f4_test$X1RSCALK1, center = T, scale = T)
f4_test$X2RSCALK1 <- scale(f4_test$X2RSCALK1, center = T, scale = T)
f4_test$X4RSCALK1 <- scale(f4_test$X4RSCALK1, center = T, scale = T)
f4_test$X1MSCALK1 <- scale(f4_test$X1MSCALK1, center = T, scale = T)
f4_test$X2MSCALK1 <- scale(f4_test$X2MSCALK1, center = T, scale = T)
f4_test$X4MSCALK1 <- scale(f4_test$X4MSCALK1, center = T, scale = T)

f4_test$T1RARS <- scale(f4_test$T1RARS, center = T, scale = T)
f4_test$T2RARS <- scale(f4_test$T2RARS, center = T, scale = T)
f4_test$T4RARS <- scale(f4_test$T4RARS, center = T, scale = T)
f4_test$T1MARS <- scale(f4_test$T1MARS, center = T, scale = T)
f4_test$T2MARS <- scale(f4_test$T2MARS, center = T, scale = T)
f4_test$T4MARS <- scale(f4_test$T4MARS, center = T, scale = T)

# Remove ARS item scores
f4_test <- f4_test[,-c(20:70)]

# Calculate difference scores
f4_test$T1RDIFF <- f4_test$T1RARS - f4_test$X1RSCALK1
f4_test$T2RDIFF <- f4_test$T2RARS - f4_test$X2RSCALK1
f4_test$T4RDIFF <- f4_test$T4RARS - f4_test$X4RSCALK1
f4_test$T1MDIFF <- f4_test$T1MARS - f4_test$X1MSCALK1
f4_test$T2MDIFF <- f4_test$T2MARS - f4_test$X2MSCALK1
f4_test$T4MDIFF <- f4_test$T4MARS - f4_test$X4MSCALK1

# Damn figure out some outliers
# Go here if outliers are a further issue: 
# https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/

hist(f4_test$X1RSCALK1)
hist(f4_test$X2RSCALK1)
hist(f4_test$X4RSCALK1)
hist(f4_test$X1MSCALK1)
hist(f4_test$X2MSCALK1)
hist(f4_test$X4MSCALK1)

# There is one outlier in X1MSCALK1 (Just assuming, didn't statistically test)
# SO I guess I should test
# ::crosses fingers:: ::pours out an offering of full-caf coffee::
source("http://goo.gl/UUyEzD")
outlierKD(f4, X1MSCALK1)
# IT WORKS MAHAHAHAHA
# This identifies way more outliers than just the one that's way off the map

# Gonna just replace it with NA for now
which(f4_test$X1MSCALK1 > 5)
f4_test[f4_test$X1MSCALK1 == 5.859013] <- NA
# OR
f4_test[5606,17]<- NA
f4_test[5606,17]

# Okay let's look at difference scores
hist(f4_test$T1RDIFF)
hist(f4_test$T2RDIFF)
hist(f4_test$T4RDIFF)
hist(f4_test$T1MDIFF)
hist(f4_test$T2MDIFF)
hist(f4_test$T4MDIFF)





# Multiple regression

# First check each independent variable's relationship to the dependent variable


# asterisk is interaction
# + is additive 

lm.1 <- lm(X4MSCALK1 ~ X1MSCALK1 + T1MDIFF + X_CHSEX_R + X_HISP_R + X_WHITE_R + X_BLACK_R + 
             X_ASIAN_R +  X_AMINAN_R + X_HAWPI_R + X_MULTR_R, na.action=na.omit, data=f3_test)
summary(lm.1)
plot(f3_test$T1MDIFF, f3_test$X4MSCALK1)


# lm.2 <- lm(X4MSCALK1 ~ T1MDIFF * X_CHSEX_R, na.action=na.omit, data=lm.1$model)
# summary(lm.2)
# anova(lm.1, lm.2)

# For complete cases: 
lm.3 <- lm(X4MSCALK1 ~ X1MSCALK1 * T1MDIFF * X_CHSEX_R, na.action=na.omit, data=f4_test)
summary(lm.3)
plot(f4_test$T1MDIFF, f4_test$X4MSCALK1)

boxplot(X4MSCALK1 ~ X_RACETH_R, data=f3_test, xlab="Race", ylab="Time 4 Math ARS")
boxplot(X1MSCALK1 ~ X_RACETH_R, data=f3_test, xlab="Race", ylab="Time 4 Math ARS")
# Races: 1 = White, Non-Hispanic
#        2 = Black, Non-Hispanic
#        3 = Hispanic, Race Specified
#       (4 = Hispanic, Race Not Specified)
#        5 = Asian, Non-Hispanic
#       (6 = Native Hawaiian or Pacific Islander, Non-Hispanic)
#        7 = American Indian or Alaska native, Non-Hispanic
#        8 = 2 or more races, non-hispanic

str(f3_test$X_RACETH_R)
f3_test$RACE_AS_FACTOR <- as.factor(f3_test$X_RACETH_R)
levels(f3_test$RACE_AS_FACTOR) <- c("White, Non-Hispanic", "Black, Non-Hispanic", "Hispanic, Race Specified", 
                                "Asian, Non-Hispanic", "American Indian or Alaska native, Non-Hispanic", 
                                "2 or more races, Non-Hispanic")
lm.4 <- lm(X4MSCALK1 ~ T1MDIFF + X_CHSEX_R + X_RACETH_R, data=f3_test)
anova(lm.4)

# Export files as .rda
save(f4, file="complete.cases.rda")
save(f2, file="sort.of.complete.cases.rda")
save(f1, file="all.data.rda")
getwd()


