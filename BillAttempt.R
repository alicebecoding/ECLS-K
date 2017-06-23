
library(readr)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(readxl)
library(ggplot2)
library(mice)

small_database <- read_csv("Z:/wgehring/Drobo/Alice/ECLS-K/Mine/small_database.csv")
sm_no6 <- read_excel("Z:/wgehring/Drobo/Alice/ECLS-K/Mine/small_no6.xlsx")
describe(sm_no6)

describe(small_database)
describe(small_database$T1CMPSEN)

# create a database that gets rid of cases with NAs, 6s and -9s
# but this only does it for a few variables
# so it's no good
# s <- small_database %>% filter(!is.na(small_database[,12:20]), T1CMPSEN < 6, T1CMPSEN > 0,
#                               !is.na(X1RTHETK1), X1RTHETK1 > -9,
#                               !is.na(X_CHSEX_R), X_CHSEX_R > 0,
#                               !is.na(X4RTHETK1), X4RTHETK1 > -9)

# test has cases with 6s removed from only column 12
# so no good
# test <- small_database_dt[seq(12:61)<6]


# subset only ARS rows; replace all 6s and -9s in dataset with NAs: YASSSSS
sm_ARS <- small_database[,12:62]
sm_ARS[sm_ARS == "6" ] = NA
sm_ARS[sm_ARS == "-9"] = NA

# create ARS averages in ARS dataset, 
sm_ARS$T1RARS <- rowMeans(sm_ARS[,c("T1CMPSEN","T1STORY","T1LETTER","T1PRDCT","T1READS","T1USESTR","T1WRITE","T1CMPSTR","T1PRINT")],na.rm=T)
sm_ARS$T2RARS <- rowMeans(sm_ARS[,c("T2CMPSEN","T2STORY","T2LETTER","T2PRDCT","T2READS","T2USESTR","T2WRITE","T2CMPSTR","T2PRINT")],na.rm=T)
sm_ARS$T4RARS <- rowMeans(sm_ARS[,c("T4RLVINF","T4INTSTY","T4REGVWL","T4IRGVWL","T4RD1IND","T4RD1FLN","T4CMPSES","T4CONPRN","T4WRTSKIL")],na.rm=T)
sm_ARS$T1MARS <- rowMeans(sm_ARS[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH","T1MEASU","T1STRAT","T1FRACTN")],na.rm=T)
sm_ARS$T2MARS <- rowMeans(sm_ARS[,c("T2SORTS","T2ORDER","T2RELAT","T2SOLVE","T2GRAPH","T2MEASU","T2STRAT","T2FRACTN")],na.rm=T)
sm_ARS$T4MARS <- rowMeans(sm_ARS[,c("T4PLCVL","T4WHNUM","T4COINS","T4GRPHAC","T4ESTQNT","T4MEASU","T4STRAT","T4FRACTN")],na.rm=T)


# put data frame back together
sm_ARS_NA <- cbind(small_database[,1:11],sm_ARS)
sm_ARS_NA <-cbind(sm_ARS_NA,small_database[,63:64])
View(sm_ARS_NA)

# create ARS averages in dataset with no 6s
# first subset only ARS rows
sm_ARS_no69  <- small_no6[,12:62]

# replace -9s with NAs
sm_ARS_no69[sm_ARS_no69 == "-9"] = NA

# create ARS averages in no 6s ARS dataset
sm_ARS_no69$T1RARS <- rowMeans(sm_ARS_no69[,c("T1CMPSEN","T1STORY","T1LETTER","T1PRDCT","T1READS","T1USESTR","T1WRITE","T1CMPSTR","T1PRINT")],na.rm=T)
sm_ARS_no69$T2RARS <- rowMeans(sm_ARS_no69[,c("T2CMPSEN","T2STORY","T2LETTER","T2PRDCT","T2READS","T2USESTR","T2WRITE","T2CMPSTR","T2PRINT")],na.rm=T)
sm_ARS_no69$T4RARS <- rowMeans(sm_ARS_no69[,c("T4RLVINF","T4INTSTY","T4REGVWL","T4IRGVWL","T4RD1IND","T4RD1FLN","T4CMPSES","T4CONPRN","T4WRTSKIL")],na.rm=T)
sm_ARS_no69$T1MARS <- rowMeans(sm_ARS_no69[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH","T1MEASU","T1STRAT","T1FRACTN")],na.rm=T)
sm_ARS_no69$T2MARS <- rowMeans(sm_ARS_no69[,c("T2SORTS","T2ORDER","T2RELAT","T2SOLVE","T2GRAPH","T2MEASU","T2STRAT","T2FRACTN")],na.rm=T)
sm_ARS_no69$T4MARS <- rowMeans(sm_ARS_no69[,c("T4PLCVL","T4WHNUM","T4COINS","T4GRPHAC","T4ESTQNT","T4MEASU","T4STRAT","T4FRACTN")],na.rm=T)


# put data frame back together
sm_ARS_no69_NA <- cbind(sm_no6[,1:11],sm_ARS_no69,sm_no6[,63:64])
View(sm_ARS_no69_NA)

# histograms of ARS scores in full vs no 6 datasets
hist(sm_ARS_NA$T1RARS,breaks=9,xlab="Academic Rating Scale Score",ylab="Frequency",main="ARS Time 1 Reading scores - Full dataset")
hist(sm_ARS_no69_NA$T1RARS,breaks=9,xlab="Academic Rating Scale Score",ylab="Frequency",main="ARS Time 1 Reading scores - Complete cases")

hist(sm_ARS_NA$T2RARS,breaks=9,xlab="Academic Rating Scale Score",ylab="Frequency",main="ARS Time 2 Reading scores - Full dataset")
hist(sm_ARS_no69_NA$T2RARS,breaks=9,xlab="Academic Rating Scale Score",ylab="Frequency",main="ARS Time 2 Reading scores - Complete cases")

hist(sm_ARS_NA$T4RARS,breaks=9,xlab="Academic Rating Scale Score",ylab="Frequency",main="ARS Time 4 Reading scores - Full dataset")
hist(sm_ARS_no69_NA$T4RARS,breaks=9,xlab="Academic Rating Scale Score",ylab="Frequency",main="ARS Time 4 Reading scores - Complete cases")

hist(sm_ARS_NA$T1MARS,breaks=9,xlab="Academic Rating Scale Score",ylab="Frequency",main="ARS Time 1 Math scores - Full dataset")
hist(sm_ARS_no69_NA$T1MARS,breaks=9,xlab="Academic Rating Scale Score",ylab="Frequency",main="ARS Time 1 Math scores - Complete cases")

hist(sm_ARS_NA$T2MARS,breaks=9,xlab="Academic Rating Scale Score",ylab="Frequency",main="ARS Time 2 Math scores - Full dataset")
hist(sm_ARS_no69_NA$T2MARS,breaks=9,xlab="Academic Rating Scale Score",ylab="Frequency",main="ARS Time 2 Math scores - Complete cases")

hist(sm_ARS_NA$T4MARS,breaks=9,xlab="Academic Rating Scale Score",ylab="Frequency",main="ARS Time 4 Math scores - Full dataset")
hist(sm_ARS_no69_NA$T4MARS,breaks=9,xlab="Academic Rating Scale Score",ylab="Frequency",main="ARS Time 4 Math scores - Complete cases")

# tryin some t-tests
# guess I start with an F-test for homoskedasticity
a <- sm_ARS_NA$T1RARS
b <- sm_ARS_no69_NA$T1RARS
var.test(a,b)
# then I do t-test
t.test(a,b,var.equal=T)

# same for other times and math
c <- sm_ARS_NA$T2RARS
d <- sm_ARS_no69_NA$T2RARS
var.test(c,d)
t.test(c,d,var.equal=F)

e <- sm_ARS_NA$T4RARS
f <- sm_ARS_no69_NA$T4RARS
var.test(e,f)
t.test(e,f,var.equal=F)

g <- sm_ARS_NA$T1MARS
h <- sm_ARS_no69_NA$T1MARS
var.test(g,h)
t.test(g,h,var.equal=T)

i <- sm_ARS_NA$T2MARS
j <- sm_ARS_no69_NA$T2MARS
var.test(i,j)
t.test(i,j,var.equal=F)

k <- sm_ARS_NA$T4MARS
l <- sm_ARS_no69_NA$T4MARS
var.test(k,l)
t.test(k,l,var.equal=T)

# now I will compare gender and income between groups
m <- sm_ARS_NA$X_CHSEX_R
n <- sm_ARS_no69_NA$X_CHSEX_R
var.test(m,n)
t.test(m,n,var.equal = T)

o <- sm_ARS_NA$X2INCCAT_I
p <- sm_ARS_no69_NA$X2INCCAT_I
var.test(o,p)
t.test(o,p,var.equal = T)

# p <- ggplot(s, aes(x = T1CMPSEN, y = X1RTHETK1, color = as.factor(X_CHSEX_R))) +
#   geom_jitter(alpha = .5) + 
#   geom_smooth(se = F, method = "lm")
# plot(p)  


# asterisk is interaction
# + is additive 
lm.1 <- lm(formula = X1RTHETK1 ~ T1CMPSEN * X_CHSEX_R, na.action = na.omit, data = s)
summary(lm.1)
anova(lm.1)


p <- ggplot(s, aes(x = X1RTHETK1, y = X4RTHETK1, color = as.factor(X_CHSEX_R))) +
  geom_jitter(alpha = .5) + 
  geom_smooth(se = F, method = "lm")
plot(p)  


p <- ggplot(s, aes(x = X1RTHETK1, y = X4RTHETK1, color = as.factor(X_CHSEX_R))) +
  geom_jitter(alpha = .5) + 
  geom_smooth(se = F, method = "lm") + facet_wrap(~ T1CMPSEN)
plot(p)  



lm.2 <- lm(formula = X4RTHETK1 ~ X1RTHETK1 * T1CMPSEN * X_CHSEX_R, na.action = na.omit, data = s)
summary(lm.2)
anova(lm.2)




