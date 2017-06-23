# Multivariate Imputation with Chained Equations, aka mice

library(mice)
library(lattice)
library(VIM)
nhanes <- nhanes

# shows a table of missing patterns
md.pattern(nhanes)

# output like so: 
#     # with 0 missing | columns showing missing pattern | total missings with this particular pattern
      # with 1 missing | columns showing missing pattern | total missings with this particular pattern
      # with 1 missing | columns showing missing pattern | total missings with this particular pattern
      # with 2 missing | columns showing missing pattern | total missings with this particular pattern
#                        total missing from each column  | total missing from data frame

# I don't get what this does: 
p <- md.pairs(nhanes)
p

# VIM package for plotting incomplete data
install.packages("VIM")
library(VIM)
marginplot(nhanes[,c("chl","bmi")], col=mdc(1:2), cex=1.2, cex.lab = 1.2, cex.numbers = 1.3, pch=19)
# this plots the points for which both chosen variables are present. 
# along the y-axis are points for which the y variable is known but the x missing
# vice versa for x-axis
# the number in the corner is the number of cases missing both. 
# the number above that on the y-axis is how many cases are missing the y-variable
# the number to the right on the x-axis is how many cases are missing the x-variable
# the box plots on the sides show the distribution of those with both values vs missing one. 
# If the data is MCAR they should look the same. 

# run the imputation with default settings
imp <- mice(nhanes, seed=23109)
            
# summary of the imputation
print(imp)

# show the imputations for a particular variable:
imp$imp$bmi
# rows are entries missing the variable. columns are m imputations. 

# to call the complete data sets: 
complete(imp) # delivers first imputation
complete(imp, 2) # delivers the second
# etc

# inspect the distributions of the original and imputed data:
library(lattice)
stripplot(imp, pch = 20, cex = 1.2)
# this shows the spread of each individual variable for each imputation. 

# create a scatterplot of chl and bmi for original data and for each imputation. 
xyplot(imp, bmi~chl | .imp, pch=20, cex=1.4)

# linear regression of cholesterol on age and bmi. 
fit <- with(imp, lm(chl ~ age + bmi))
print(pool(fit))
round(summary(pool(fit)),2)
# shows estimates, se, t, df, p, confidence interval, number missing, fraction of missing information, 
# and lambda which is the proportion of variance attributable to the missing data. 

# Rerun with m = 50
one <- Sys.time()
imp50 <- mice(nhanes, m=50, seed=23109)
two <- Sys.time()
fifty <- two-one

# ...and compare time to m = 5 to make sure it's really doing it.
three <- Sys.time()
imp <- mice(nhanes, seed=23109)
four <- Sys.time()
five <- (four-three)

five
fifty

# Run linear model for imp50, then summarize both
fit50 <- with(imp50, lm(chl ~ age + bmi))
print(pool(fit50))
round(summary(pool(fit50)),2)
round(summary(pool(fit)),2)

# correlate variables to one another to explore what to use as predictors
round(cor(nhanes, use="pair"),3)

print(nhanes)

# Correlates *whether or not* a variable is NA with the value of another variable. 
round(cor(y=nhanes, x=!is.na(nhanes), use="pair"), 3)

# Calculates the proportion of usable data to predict one variable from another
prop.usable <- md.pairs(nhanes)
round(prop.usable$mr/(prop.usable$mr+prop.usable$mm), 3)

# This comes up with a sensible predictor matrix automatically from the above diagnostics
quickpred(nhanes)
# row name is predicted by column name (age is not predicted by anything because it's complete)

# To use quickpred in the main mice function:
imp <- mice(nhanes, pred=quickpred(nhanes, minpuc=0.25, include="age"))
# puc is proportion of usable cases. can always include certain variables. 


# preserving transformations:
# imputing BMI from log(chl) instead of chl
nhanes2.ext <- cbind(nhanes2, lchl = log(nhanes2$chl))
ini <- mice(nhanes2.ext, max=0,print=F) # creates a blank imputation form
meth <- ini$meth # makes the method an object
meth["lchl"] <- "~log(chl)" # sets the method of predicting lchl to be the log of chl
pred <- ini$pred # makes the prediction matrix an object
pred[c("hyp", "chl"),"lchl"] <- 0 # sets hyp and chl to not be predicted by lchl
pred["bmi","chl"] <- 0 # sets chl to not predict bmi
pred

# run imputation with new info
imp <- mice(nhanes2.ext, meth=meth, pred=pred, seed=38788, print=F)
head(complete(imp))

# imputing derived variables that should remain synchronized
md.pattern(boys[,c("hgt","wgt","bmi")])

# bmi has to maintain its mathematical relation to height and weight. 
# set the method of predicting BMI to the equation to calculate it, using the ~I argument. 
init <- mice(boys, max=0, print=F)
meth <- init$meth
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
pred <- init$pred
pred[c("wgt","hgt","hc","reg"),"bmi"] <- 0 # makes weight, height, hc and gen not predicted by bmi
pred[c("gen","phb","tv"),c("hgt","wgt","hc")] <- 0 # makes variables that are predicted by bmi not also
# predicted by height and weight - should only be predicted by either/or
pred

# runs the imputation with the new info
imp.idx <- mice(boys, pred=pred, meth=meth, maxit = 20, seed=9212, print=F)
head(complete(imp.idx)[is.na(boys$bmi),],3)

# using a sum score to predict
ini <- mice(cbind(boys, mat=NA), max=0, print=F) # adds a column for maturation score, all NAs
meth <- ini$meth
meth["mat"] <- "~I(as.integer(gen)+as.integer(phb)+as.integer(cut(tv,breaks=c(0,3,6,10,15,20,25))))"
# creates a sum score, mat, made of gen, phb and tv
# since gen and phb are factors, used as.integer
# used cut() to divide tv into ordered categories
# this returns integers for the imputed values, too. 
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
pred <- ini$pred
pred[c("bmi", "gen", "phb", "tv"), "mat"] <- 0
pred[c("hgt", "wgt", "hc", "reg"), "mat"] <- 1
pred[c("hgt", "wgt", "hc", "reg"), c("gen", "phb", "tv")] <- 0
pred[c("wgt", "hgt", "hc", "reg"), "bmi"] <- 0
pred[c("gen", "phb", "tv"), c("hgt", "wgt", "hc")] <- 0
pred

# generate the imputation
imp.sum <- mice(cbind(boys, mat=NA), pred=pred, meth=meth, maxit=20, seed=10948, print=F)
# plot
xyplot(imp.sum, mat ~ age | .imp, na=gen | phb | tv, subset = .imp == 1, ylab="Maturation score",
       xlab="Age (years)")

# Adds a variable for the interaction of bmi and chl
nhanes2.ext <- cbind(nhanes2, bmi.chl=NA)
ini <- mice(nhanes2.ext, max=0, pri=F)
meth <- ini$meth
meth[c("bmi.chl")] <- "~I((bmi-25)*(chl-200))"
pred <- ini$pred
pred[c("bmi","chl"), "bmi.chl"] <- 0
imp <- mice(nhanes2.ext, meth=meth, pred=pred, seed=1, maxit=100, print=F)


# setting post processing restrictions
ini <- mice(cbind(boys, mat=NA), max=0, print=F) 
meth <- ini$meth
meth["mat"] <- "~I(as.integer(gen)+as.integer(phb)+as.integer(cut(tv,breaks=c(0,3,6,10,15,20,25))))"
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
pred <- ini$pred
pred[c("bmi", "gen", "phb", "tv"), "mat"] <- 0
pred[c("hgt", "wgt", "hc", "reg"), "mat"] <- 1
pred[c("hgt", "wgt", "hc", "reg"), c("gen", "phb", "tv")] <- 0
pred[c("wgt", "hgt", "hc", "reg"), "bmi"] <- 0
pred[c("gen", "phb", "tv"), c("hgt", "wgt", "hc")] <- 0
pred

# this says for age under 5, the levels of gen and phb will be set to the first level. 
post <- ini$post
post["gen"] <- "imp[[j]][p$data$age[!r[,j]]<5,i] <- levels(boys$gen)[1]"
post["phb"] <- "imp[[j]][p$data$age[!r[,j]]<5,i] <- levels(boys$phb)[1]"
post["tv"] <- "imp[[j]][p$data$age[!r[,j]]<5,i] <- 1"

imp <- mice(cbind(boys, mat = NA), pred = pred, meth = meth, post = post, maxit = 10, print = FALSE)
xyplot(imp, mat ~ age | .imp, na=gen | phb | tv, subset = .imp == 1, ylab="Maturation score",
       xlab="Age (years)")

# Visiting scheme: revisiting 3.4
nhanes2.ext <- cbind(nhanes2, bmi.chl=NA)
imp <- mice(nhanes2.ext, max=0, pri=F)
meth <- imp$meth
meth[c("bmi.chl")] <- "~I((bmi-25)*(chl-200))"
pred <- imp$pred
pred[c("bmi","chl"), "bmi.chl"] <- 0
imp <- mice(nhanes2.ext, meth=meth, pred=pred, seed=1, maxit=100, print=F)

# the problem is that bmi.chl isn't synched with bmi before hyp is imputed. Add another visit 
# to bmi.chl directly after bmi, and keep the visit to bmi.chl after chl. 
vis <- imp$vis
vis <- append(vis, vis[4], 1) # adds a copy of the 4th value of the vector in after the 1st value
vis
imp <- mice(nhanes2.ext, meth=meth, pred=pred, vis=vis, print=F)

# a more efficient way to order variables: 
imp <- mice(nhanes2.ext, meth=meth, pred=pred, vis=c(2,4,5,3), print=F)

# can do iterations step by step, to split computations into smaller parts. 
imp <- mice(nhanes, maxit = 4, seed=44612, print=F)
imp1 <- mice(nhanes, maxit = 1, seed=44612, print=F)
a <- runif(10)
imp2 <- mice.mids(imp1, maxit=3, print=F)
all(imp$imp$bmi == imp2$imp$bmi)
# imp and imp2 are the same - imp1 was the first step, imp2 finished it. 

# Assessing convergence or lack thereof
ini <- mice(boys, max=0, print=F)
meth <- ini$meth
meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
imp2 <- mice(boys, meth=meth, maxit=20, print=F, seed=9212)
plot(imp2, c("hgt","wgt","bmi")) # this convergence is bad because bmi feeds back into height 
# and weight. It is particularly bad for weight. 

# make bmi not predict height or weight
pred <- ini$pred
pred[c("hgt", "wgt"), "bmi"] <- 0
imp3 <- mice(boys, meth=meth, pred=pred, maxit=20, print=F, seed=9212, vis=)
plot(imp3, c("hgt", "wgt", "bmi"))
# this still isn't great because bmi and hgt/wgt are predicting some of the same variables

# make sure bmi and hgt/wgt aren't predicting any of the same variables
pred.1 <- ini$pred
pred.1[c("hgt", "wgt"), "bmi"] <- 0
pred.1[c("gen", "phb", "tv"), c("hgt", "wgt", "hc")] <- 0
pred.1[c("hc","reg"), "bmi"] <- 0

imp4 <- mice(boys, meth=meth, pred=pred.1, maxit=20, print=F, seed=9212)
plot(imp4, c("hgt", "wgt", "bmi"))
# this one is good. 

# these plots default to mean and SD to monitor convergence. 
# to monitor convergence of other stats, write a function

# This will calculate Kendall's T, association between two variables. 

m <- 5
t <- 20
imp.kendall <- mice(boys, m = m, meth = imp.idx$meth, pred = imp.idx$pred, maxit = 0, print = F)

tau <- matrix(NA, nrow = t, ncol = m)
for(i in 1:t){
  if (i==1) set.seed(9212)
  imp.kendall <- mice.mids(imp.kendall, maxit=1, print=F)
  x <- complete(imp.kendall, "repeated")[,paste("gen", 1:m, sep=".")]
  y <- complete(imp.kendall, "repeated")[,paste("phb", 1:m, sep=".")]
  xn <- as.data.frame(lapply(x, as.numeric))
  yn <- as.data.frame(lapply(y, as.numeric))
  tau[i, ] <- diag(cor(xn,yn,method="kendall"))
}

matplot(x=1:t, y=tau, xlab="Iteration", type="l")


# plotting densities of imputed variables
library(lattice)
densityplot(imp.kendall, scales=list(x=list(relation="free")),layout=c(5,1))

# Plot propensity scores
hc.na <- is.na(boys$hc)
fit.hc <- with(imp.kendall, glm(hc.na ~ age + wgt + hgt + reg, family=binomial))
ps <- rep(rowMeans(sapply(fit.hc$analyses, fitted.values)),6)
xyplot(imp.kendall, hc~ps | .imp, pch=c(1,20), cex=c(0.8, 1,2), 
       xlab="Probability that head circumference is missing", 
       ylab="Head circumference (cm)", scales=list(tick.number=3))
# By definition there will be more imputed values for higher propensity of missingness, 
# but the general shape of imputed vs real values should fit. 

hc <- complete(imp.kendall, "long", TRUE)$hc
fit <- lm(hc ~ poly(ps, 4))
densityplot(~residuals(fit), group=hc.na, plot.points=F, ref=T, scales=list(y=list(draw=F)),
            par.settings=simpleTheme(col.line=rep(mdc(1:2))), xlab="Residuals of regression of hc 
            on propensity score", lwd=2)

?complete


# Running analyses on mids: creates a mira object

imp <- mice(nhanes2, seed = 99210, print = FALSE)
fit <- with(imp, lm(chl ~ age + bmi)) # with() calls the fxn with.mids()
summary(pool(fit))
  
fit$ana[[3]] # calls results of a single imputation

# create an expression to apply to the imp/mids object. 
expr <- expression(ov <- cut(bmi, c(10, 25, 50)), table(age,ov)) # take ov, an object made of BMI cut 
# into two categories, and make a table of it by age. 
fit <- with(imp, eval(expr)) # this applies that expression to all the imputations
fit$analyses[c(2,5)] # and this looks at the results for imputations 2 and 5. 



# pooling data: mira to mipo

fit <- with(imp, lm(chl ~ age + bmi))
est <- pool(fit)

# get info on the pooled data
pool(object=fit) # basic
summary(pool(fit)) # more details

# pool() forks for objects that have both coef() and vcov() methods. Find those here:
methods(coef)
methods(vcov)

# other options:
pool.scalar
pool.r.squared # combines estimates for R^2 and adjusted R^2
pool.compare # compares two nested models fitted on imputed data - wald test and likelihood ratio test
# can be used to test whether one or more variables should be present in model

imp <- mice(nhanes2, print=F, m=50, seed=219)
fit0 <- with(data = imp, expr = lm(bmi ~ age + hyp))
fit1 <- with(data = imp, expr = lm(bmi ~ age + hyp + chl))
stat <- pool.compare(fit1, fit0, method = "Wald")
stat$p
