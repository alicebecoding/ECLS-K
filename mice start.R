# mice with ECLS-K start

library(mice)
library(ggplot2)

# mice source code URL: http://www.restore.ac.uk/PEAS/ex6datafiles/program_code/micesource.R

# TO DO: Clean data
  # 6s and NAs
  # Run thru script, use max of 4 6s to start. 



######################################################################################################
######## THE PROCESS #################################################################################
######################################################################################################
############ DATA IMPORT & CLEANING ##################################################################
######################################################################################################



may_data <- read.csv("Z:/wgehring/Drobo/Alice/ECLS-K/Data/med_db_may.csv")
names(may_data)[1] <- "CHILDID"
# utils::View(may_data)

# recode -9s as NAs
may_data[may_data==-9] <- NA
# check whole data.frame for -9s
apply(may_data, 2, function(x) any(x==-9))

# replace 6s with NAs
may_data_ARS <- may_data[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH","T1MEASU",
                            "T1STRAT","T1FRACTN","T2SORTS","T2ORDER","T2RELAT","T2SOLVE",
                            "T2GRAPH","T2MEASU","T2STRAT","T2FRACTN","T4PLCVL","T4WHNUM",
                            "T4COINS","T4GRPHAC","T4ESTQNT","T4MEASU","T4STRAT","T4FRACTN")]
may_data_ARS[may_data_ARS==6] <- NA
may_data <- cbind(may_data[c("CHILDID","X_CHSEX_R","X1KAGE_R","X_RACETH_R","X4SESL_I","X1MSCALK1",
                           "X2MSCALK1","X4MSCALK1","X1MTHETK1","X2MTHETK1","X4MTHETK1")], 
                  may_data_ARS, may_data[c("X1TCHEXT","X1TCHAPP","A1FULDAY","A1HALFAM","A1HALFPM",
                                         "A1BOTHCL","A1AHRSDA","A1PHRSDA","A1DHRSDA","A1ADYSWK",
                                         "A1PDYSWK","A1DDYSWK","A1ATOTAG","A1PTOTAG","A1DTOTAG",
                                         "A1ABEHVR","A1PBEHVR","A1DBEHVR","A1ANMELL","A1PNMELL",
                                         "A1DNMELL","A1CNTRLC","A1CLSSIZ","A1YRSTCH","A1CLSSMN",
                                         "A1HIGHQL","A2AENROL","A2PENROL","A2DENROL","A2AJOINE",
                                         "A2PJOINE","A2DJOINE","A2ALEFTL","A2PLEFTL","A2DLEFTL",
                                         "A2ATARDY","A2PTARDY","A2DTARDY","A2AABSEN","A2PABSEN",
                                         "A2DABSEN","A2ABEHVR","A2PBEHVR","A2DBEHVR","A2ASPECN",
                                         "A2PSPECN","A2DSPECN","A2ASPCIA","A2PSPCIA","A2DSPCIA",
                                         "A2AMORE","A2PMORE","A2DMORE","A2LUNCH","A2TPLYIN",
                                         "A2TPLOUT","A2REGWRK","A2SPEDWK","A2ESLWRK","A2REGOTH",
                                         "A2SPEDOT","A2ESLOTH","A2VOLIT","A2VOLOT","A2STNDRD",
                                         "A2CMRCLT","A2TCHRMD","A2IGRPRJ","A2TXTBKT","A2WRKSHT",
                                         "A2WRKSMP","A2OBSOBJ","A2MISBHV")])

# have -1s as NAs
# "A1AHRSDA","A1PHRSDA","A1DHRSDA","A1ADYSWK","A1PDYSWK","A1DDYSWK","A1ATOTAG","A1PTOTAG","A1DTOTAG",
# "A1ABEHVR","A1PBEHVR","A1DBEHVR","A1ANMELL","A1PNMELL","A1DNMELL","A1HIGHQL","A2AENROL","A2PENROL",
# "A2DENROL","A2AJOINE","A2PJOINE","A2DJOINE","A2ALEFTL","A2PLEFTL","A2DLEFTL","A2ATARDY","A2PTARDY",
# "A2DTARDY","A2AABSEN","A2PABSEN","A2DABSEN","A2ABEHVR","A2PBEHVR","A2DBEHVR","A2ASPECN","A2PSPECN",
# "A2DSPECN","A2ASPCIA","A2PSPCIA","A2DSPCIA","A2AMORE","A2PMORE","A2DMORE"
# already coded as NAs

# Change child sex and race to factor variables
may_data$X_CHSEX_R <- as.factor(may_data$X_CHSEX_R)
levels(may_data$X_CHSEX_R) <- c(0,1)
# 0 is male, 1 is female
may_data$X_RACETH_R <- as.factor(may_data$X_RACETH_R)

# Run data through NA cleaning function: This time, 4 or more NAs is cutoff
may_six <- na_cleaning(may_data, 2, col1.start = 12, col1.end = 19)
may_five <- na_cleaning(may_data, 3, col1.start = 12, col1.end = 19)
may_four <- na_cleaning(may_data, 4, col1.start = 12, col1.end = 19)
may_three <- na_cleaning(may_data, 5, col1.start = 12, col1.end = 19)
may_two <- na_cleaning(may_data, 6, col1.start = 12, col1.end = 19)

# Create average ARS scores 
may_six$T1MARSAV <- rowMeans(may_six[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH","T1MEASU",
                                        "T1STRAT","T1FRACTN")],na.rm = T)
may_six$T1MARSZ <- scale(may_six$T1MARSAV, center=T, scale=T)
may_six$T1MDIFF <- may_six$T1MARSZ-may_six$X1MTHETK1
# utils::View(may_six)
# summary(may_six$T1MARSAV)

may_five$T1MARSAV <- rowMeans(may_five[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH","T1MEASU",
                                          "T1STRAT","T1FRACTN")],na.rm = T)
may_five$T1MARSZ <- scale(may_five$T1MARSAV, center=T, scale=T)
may_five$T1MDIFF <- may_five$T1MARSZ-may_five$X1MTHETK1
# utils::View(may_five)
# summary(may_five$T1MARSAV)

may_four$T1MARSAV <- rowMeans(may_four[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH","T1MEASU",
                                          "T1STRAT","T1FRACTN")],na.rm = T)
may_four$T1MARSZ <- scale(may_four$T1MARSAV, center=T, scale=T)
may_four$T1MDIFF <- may_four$T1MARSZ-may_four$X1MTHETK1
# utils::View(may_four)
# summary(may_four$T1MARSAV)

may_three$T1MARSAV <- rowMeans(may_three[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH","T1MEASU",
                                            "T1STRAT","T1FRACTN")],na.rm = T)
may_three$T1MARSZ <- scale(may_three$T1MARSAV, center=T, scale=T)
may_three$T1MDIFF <- may_three$T1MARSZ-may_three$X1MTHETK1
# utils::View(may_three)
# summary(may_three$T1MARSAV)

may_two$T1MARSAV <- rowMeans(may_two[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH","T1MEASU",
                                        "T1STRAT","T1FRACTN")],na.rm = T)
may_two$T1MARSZ <- scale(may_two$T1MARSAV, center=T, scale=T)
may_two$T1MDIFF <- may_two$T1MARSZ-may_two$X1MTHETK1
# utils::View(may_two)
# summary(may_two$T1MARSAV)

# compare basic density plots of ARS averages
# plot(density(na.omit(may_six$T1MARSZ)))
# plot(density(na.omit(may_five$T1MARSZ)))
# plot(density(na.omit(may_four$T1MARSZ)))
# plot(density(na.omit(may_three$T1MARSZ)))
# plot(density(na.omit(may_two$T1MARSZ)))

# set up an ANOVA
# six <- may_six$T1MARSAV
# five <- may_five$T1MARSAV
# four <- may_four$T1MARSAV
# three <- may_three$T1MARSAV
# two <- may_two$T1MARSAV

# df.m <- data.frame(Score=c(two, three, four, five, six), NumARS =factor(rep(c("two", 
#                      "three", "four", "five", "six"), times=c(length(two), length(three),
#                       length(four), length(five), length(six)))))
# fm1 <- aov(Score~NumARS, data=df.m)
# anova(fm1)

# Run a t-test of the most different-looking
# t.test(may_six$T1MARSAV, may_two$T1MARSAV)

# Replacing ARS columns with average score
may_six <- cbind(may_six[,1:11], T1MARSAV=may_six[,109], T1MARSZ=may_six[,110], 
                 T1MDIFF = may_six[,111], may_six[,36:108])
may_five <- cbind(may_five[,1:11], T1MARSAV=may_five[,109], T1MARSZ=may_five[,110], 
                  T1MDIFF = may_five[,111], may_five[,36:108])
may_four <- cbind(may_four[,1:11], T1MARSAV=may_four[,109], T1MARSZ=may_four[,110], 
                  T1MDIFF = may_four[,111], may_four[,36:108])
may_three <- cbind(may_three[,1:11], T1MARSAV=may_three[,109], T1MARSZ=may_three[,110], 
                   T1MDIFF = may_three[,111], may_three[,36:108])
may_two <- cbind(may_two[,1:11], T1MARSAV=may_two[,109], T1MARSZ=may_two[,110], 
                 T1MDIFF = may_two[,111], may_two[,36:108])




######################################################################################################
############ SETTING UP THE IMPUTATION PARAMETERS ####################################################
######################################################################################################



# Create the blank imputation form
blank_imp_form_6 <- mice(may_six, max=0, print=F)
meth <- blank_imp_form_6$meth
meth

# Set predictors' imp methods
meth[c("X1TCHEXT","X1TCHAPP","A1FULDAY","A1HALFAM","A1HALFPM","A1BOTHCL","A1AHRSDA",
       "A1PHRSDA","A1DHRSDA","A1ADYSWK","A1PDYSWK","A1DDYSWK","A1ATOTAG","A1PTOTAG",
       "A1DTOTAG","A1ABEHVR","A1PBEHVR","A1DBEHVR","A1ANMELL","A1PNMELL","A1DNMELL",
       "A1CNTRLC","A1CLSSIZ","A1YRSTCH","A1CLSSMN","A1HIGHQL","A2AENROL","A2PENROL",
       "A2DENROL","A2AJOINE","A2PJOINE","A2DJOINE","A2ALEFTL","A2PLEFTL","A2DLEFTL",
       "A2ATARDY","A2PTARDY","A2DTARDY","A2AABSEN","A2PABSEN","A2DABSEN","A2ABEHVR",
       "A2PBEHVR","A2DBEHVR","A2ASPECN","A2PSPECN","A2DSPECN","A2ASPCIA","A2PSPCIA",
       "A2DSPCIA","A2AMORE","A2PMORE","A2DMORE","A2LUNCH","A2TPLYIN","A2TPLOUT",
       "A2REGWRK","A2SPEDWK","A2ESLWRK","A2REGOTH","A2SPEDOT","A2ESLOTH","A2VOLIT",
       "A2VOLOT","A2STNDRD","A2CMRCLT","A2TCHRMD","A2IGRPRJ","A2TXTBKT","A2WRKSHT",
       "A2WRKSMP","A2OBSOBJ","A2MISBHV")] <- ""
meth


# put predictor matrix back in mids
blank_imp_form <- mice(may_two, max=0, meth=meth, print=F)

# specify predictor matrix
pred <- blank_imp_form$predictorMatrix
# utils::View(pred)

# Sex and race predicted by most
pred[c("X_CHSEX_R","X_RACETH_R"),] <- 1
# ....except each other
pred["X_CHSEX_R","X_CHSEX_R"] <- 0
pred["X_RACETH_R","X_RACETH_R"] <- 0
# Child ID predicts nothing
pred[,"CHILDID"] <- 0
# predictors predict everything else
pred[,c("X1TCHEXT","X1TCHAPP","A1FULDAY","A1HALFAM","A1HALFPM","A1BOTHCL","A1AHRSDA",
        "A1PHRSDA","A1DHRSDA","A1ADYSWK","A1PDYSWK","A1DDYSWK","A1ATOTAG","A1PTOTAG",
        "A1DTOTAG","A1ABEHVR","A1PBEHVR","A1DBEHVR","A1ANMELL","A1PNMELL","A1DNMELL",
        "A1CNTRLC","A1CLSSIZ","A1YRSTCH","A1CLSSMN","A1HIGHQL","A2AENROL","A2PENROL",
        "A2DENROL","A2AJOINE","A2PJOINE","A2DJOINE","A2ALEFTL","A2PLEFTL","A2DLEFTL",
        "A2ATARDY","A2PTARDY","A2DTARDY","A2AABSEN","A2PABSEN","A2DABSEN","A2ABEHVR",
        "A2PBEHVR","A2DBEHVR","A2ASPECN","A2PSPECN","A2DSPECN","A2ASPCIA","A2PSPCIA",
        "A2DSPCIA","A2AMORE","A2PMORE","A2DMORE","A2LUNCH","A2TPLYIN","A2TPLOUT",
        "A2REGWRK","A2SPEDWK","A2ESLWRK","A2REGOTH","A2SPEDOT","A2ESLOTH","A2VOLIT",
        "A2VOLOT","A2STNDRD","A2CMRCLT","A2TCHRMD","A2IGRPRJ","A2TXTBKT","A2WRKSHT",
        "A2WRKSMP","A2OBSOBJ","A2MISBHV")] <- 1
# ...but are predicted by nothing. 
pred[c("CHILDID","X1TCHEXT","X1TCHAPP","A1FULDAY","A1HALFAM","A1HALFPM","A1BOTHCL","A1AHRSDA",
       "A1PHRSDA","A1DHRSDA","A1ADYSWK","A1PDYSWK","A1DDYSWK","A1ATOTAG","A1PTOTAG",
       "A1DTOTAG","A1ABEHVR","A1PBEHVR","A1DBEHVR","A1ANMELL","A1PNMELL","A1DNMELL",
       "A1CNTRLC","A1CLSSIZ","A1YRSTCH","A1CLSSMN","A1HIGHQL","A2AENROL","A2PENROL",
       "A2DENROL","A2AJOINE","A2PJOINE","A2DJOINE","A2ALEFTL","A2PLEFTL","A2DLEFTL",
       "A2ATARDY","A2PTARDY","A2DTARDY","A2AABSEN","A2PABSEN","A2DABSEN","A2ABEHVR",
       "A2PBEHVR","A2DBEHVR","A2ASPECN","A2PSPECN","A2DSPECN","A2ASPCIA","A2PSPCIA",
       "A2DSPCIA","A2AMORE","A2PMORE","A2DMORE","A2LUNCH","A2TPLYIN","A2TPLOUT",
       "A2REGWRK","A2SPEDWK","A2ESLWRK","A2REGOTH","A2SPEDOT","A2ESLOTH","A2VOLIT",
       "A2VOLOT","A2STNDRD","A2CMRCLT","A2TCHRMD","A2IGRPRJ","A2TXTBKT","A2WRKSHT",
       "A2WRKSMP","A2OBSOBJ","A2MISBHV"),] <- 0
View(pred)



######################################################################################################
############ RUNNING THE IMPUTATION ##################################################################
######################################################################################################



# Run one iteration, deliver runtime
start <- Sys.time()
eclsk.imp1 <- mice(may_two, maxit=1, m=40, meth=meth, pred=pred, seed=3375)
end <- Sys.time()
runtime <- end - start
runtime 

# plot
plot(eclsk.imp1, c("X1MSCALK1","T1MARSAV"))


# Run four more
a <- runif(10)
start <- Sys.time()
eclsk.imp2 <- mice.mids(eclsk.imp1, maxit=4)
end <- Sys.time()
runtime <- (end - start)
runtime

# plot
plot(eclsk.imp2, c("X1MSCALK1","T1MARSAV"))

# Total of 10 iterations
a <- runif(10)
start <- Sys.time()
eclsk.imp2 <- mice.mids(eclsk.imp1, maxit=5)
end <- Sys.time()
runtime <- (end - start)
runtime

# plot
plot(eclsk.imp2, c("X1MSCALK1","T1MARSAV"))



######################################################################################################
############ THE ANALYSIS ############################################################################
######################################################################################################





######################################################################################################
######################################################################################################
######################################################################################################



# Other diagnostics and shenanigans

check.predictorMatrix<-function(predictorMatrix, nmis, nvar)
{
  #	checks the predictorMatrix
  #  it can change the predictormatrix
  #  stops the program if an error is found
  #
  if(!is.matrix(predictorMatrix)) stop("Argument predictorMatrix should be a square matrix.")
  if(nvar != nrow(predictorMatrix) | nvar != ncol(predictorMatrix)) stop
  ("Argument predictorMatrix has improper dimensions.")
  if(sum(diag(predictorMatrix) != 0)) stop("The diagonal of predictorMatrix may contain only zeroes.")
  for(j in 1:nvar) {
    if(!any(predictorMatrix[j,]) & any(predictorMatrix[,j]) & nmis[j]>0) stop
    (paste("Column ", j, " is used, has missing values, but is not imputed"))
    if(nmis[j]==0 & any(predictorMatrix[j,])) {
      # warning(paste("Row ",j," of predictorMatrix is set to zero"))
      predictorMatrix[j,] <- rep(0, nvar)
    }
  }
  return(predictorMatrix)
}
nmis <- apply(is.na(may_data),2,sum)
check.predictorMatrix(pred, nmis, 108)
eclsk
str(pred)
pred_test <- blank_imp_form$predictorMatrix
str(pred_test)

?mice.mids
is.mids(eclsk.imp1)



######################################################################################################
######################################################################################################
######################################################################################################



# Notes on imputed variables: 

# Household income: 2397 imputed, 81 missing. 
# Education P1 Y1:  316 imputed, 83 missing
# Education P2 Y1:  483 imputed, 88 missing
# Prestige P1 Y1:   257 imputed, 40 missing
# Prestige P2 Y1:   417 imputed, 22 missing

# How many would complete case analysis (of just the above variables) remove?
ses_cols
ses_comp <- ses_cols[c(complete.cases(ses_cols)),]
# Leaves n=13399



######################################################################################################
######################################################################################################
######################################################################################################



# not helpful: md.pat <- md.pattern(testdata)

# Correlates *whether or not* a variable is NA with the value of another variable. 
a <- round(cor(y=test_data_2_z, x=!is.na(test_data_2_z), use="pair"), 3)
which(a>0.1)
# not really sure what to do with this


