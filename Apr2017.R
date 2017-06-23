# April 2017
# Scale scores 
# Post CSCAR consult

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

apr_data <- read.csv("small_database_april.csv", header = T)
View(apr_data)

# apr_data has:
  # sex, month and year of birth, one race variable
  # scale and theta scores for DA
  # ARS
  # nothing else
  

# FIRST need to recode ALL -9s as NA
apr_data[apr_data==-9] <- NA

# Complete cases but still 6s
apr_complete_cases <- apr_data[c(complete.cases(apr_data)),]

# check whole data.frame for NAs
apply(f2, 2, function(x) any(x==-9))
apply(f2, 2, function(x) any(is.na(x)))
# good

# create math subset
apr_math <- apr_data[,-c(6,7,8,12,13,14,18:44)]

# complete cases: has all DA scores, sex, month/year of birth, race, and was present for ARS 
# at each timepoint
apr_math_x <- apr_math[c(complete.cases(apr_math)),]

# replace 6s with NAs
# apr_math_c[apr_math_c==6] <- NA # THIS RECODES BIRTH MONTH AS NA
apr_math_ARS <- apr_math_x[,12:35]
apr_math_ARS[apr_math_ARS==6] <- NA
aprmath <- cbind(apr_math_1[1:11], apr_math_ARS)

# the below can do complete cases only for certain columns. 
apr_test <- apr_math[complete.cases(apr_math[,1:11]),]

# divide into timepoints for correlations
apr_math_ARS_1 <- aprmath[,12:19]
apr_math_ARS_2 <- aprmath[,20:27]
apr_math_ARS_3 <- aprmath[,28:35]



cor(as.matrix(apr_math_ARS_1)) # or use below:
?cor.test
?rcorr
cor(apr_math_ARS_1, use = "complete.obs")
# T1FRACTN least correlated with others - 0.37-0.66, vs all others 0.63-0.85
summary(apr_math_ARS_1)
# T1FRACTN very skewed, lots of NAs. 

cor(apr_math_ARS_2, use = "complete.obs")
# T2FRACTN least correlated with others - 0.37-0.56, vs all others 0.61-0.88
summary(apr_math_ARS_2)
# T1FRACTN has lots of NAs. 

cor(apr_math_ARS_3, use = "complete.obs")
# T4FRACTN less correlated than others, but still good - 0.55-0.66, vs all others 0.70-0.86
summary(apr_math_ARS_3)
# T4FRACTN has lots of NAs. 



################ FUNCTION FOR CLEANING DATA #############################
data_cleaning <- function(x, na.limit, col1.start, col1.end, col2.start, col2.end, col3.start, col3.end){
  temp <- x
  for(i in 1:(nrow(x))){
    if(length(x[i,(which(is.na(x[i,col1.start:col1.end])))]) <= na.limit&
       length(x[i,(which(is.na(x[i,col2.start:col2.end])))]) <= na.limit&
       length(x[i,(which(is.na(x[i,col3.start:col3.end])))]) <= na.limit){
      temp[i,(which(is.na(temp[i,])))] <- 999
    } 
  }
  test_data <<- temp[complete.cases(temp),]
}

#########################################################################

data_cleaning(testdata, na.limit = 5, col1.start = 12, col1.end = 19, col2.start = 20, col2.end = 27, col3.start = 28, col3.end = 35)
test_data_5 <- test_data

# Gathering info:
# If I accept...
# 2 or fewer NAs, n=4938, 47% of otherwise available cases
# 3 or fewer NAs, n=6864, 65% of otherwise available cases
# 4 or fewer NAs, n=8250, 78% of otherwise available cases
# 5 or fewer NAs, n=9230, 88% of otherwise available cases
# 6 or fewer NAs, n=9829, 93% of otherwise available cases

test_data_6 <- test_data


