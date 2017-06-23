# Imp for may_four


library(mice)


may_test <- read.csv("Z:/wgehring/Drobo/Alice/ECLS-K/Data/med_db_may.csv")
names(may_test)[1] <- "CHILDID"
# utils::View(may_test)

# recode -9s as NAs
may_test[may_test==-9] <- NA
# check whole data.frame for -9s
apply(may_test, 2, function(x) any(x==-9))

# replace 6s with NAs
may_test_ARS <- may_test[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH","T1MEASU",
                            "T1STRAT","T1FRACTN","T2SORTS","T2ORDER","T2RELAT","T2SOLVE",
                            "T2GRAPH","T2MEASU","T2STRAT","T2FRACTN","T4PLCVL","T4WHNUM",
                            "T4COINS","T4GRPHAC","T4ESTQNT","T4MEASU","T4STRAT","T4FRACTN")]
may_test_ARS[may_test_ARS==6] <- NA
may_test <- cbind(may_test[c("CHILDID","X_CHSEX_R","X1KAGE_R","X_RACETH_R","X4SESL_I","X1TCHCON",
                             "X1TCHEXT","X1TCHAPP","X1MSCALK1","X2MSCALK1","X4MSCALK1","X1MTHETK1",
                             "X2MTHETK1","X4MTHETK1")], 
                             may_test_ARS, 
                             may_test[c("X1TCHEXT","X1TCHAPP","A1FULDAY","A1HALFAM","A1HALFPM",
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
rm(may_test_ARS)

# Change child sex and race to factor variables
may_test$X_CHSEX_R <- as.factor(may_test$X_CHSEX_R)
levels(may_test$X_CHSEX_R) <- c(0,1)
# 0 is male, 1 is female
may_test$X_RACETH_R <- as.factor(may_test$X_RACETH_R)

# Run data through NA cleaning function: This time, 4 or more NAs is cutoff
may_4_new <- na_cleaning(may_test, 4, col1.start = 15, col1.end = 22)

# Create average ARS scores 
may_4_new$T1MARSAV <- rowMeans(may_4_new[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH","T1MEASU",
                                    "T1STRAT","T1FRACTN")],na.rm = T)
may_4_new$T1MDIFF <- (as.vector(scale(may_4_new$T1MARSAV, center=T, scale=T)))-may_4_new$X1MTHETK1


may_4_new <- may_4_new[,c("CHILDID","X_CHSEX_R","X1KAGE_R","X_RACETH_R","X4SESL_I","X1TCHCON",
                          "X1TCHEXT","X1TCHAPP","X1MSCALK1","X2MSCALK1","X4MSCALK1",
                          "T1MARSAV","T1MDIFF", 
                          "A1FULDAY","A1HALFAM","A1HALFPM",
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
                          "A2WRKSMP","A2OBSOBJ","A2MISBHV")]

utils::View(may_4_new)
# summary(may_4_new$T1MARSAV)




######################################################################################################
############ SETTING UP THE IMPUTATION PARAMETERS ####################################################
######################################################################################################

# library(mice)
# mice source code URL: http://www.restore.ac.uk/PEAS/ex6datafiles/program_code/micesource.R

# Create the blank imputation form
bimp <- mice(may_4_new, max=0, print=F)
meth <- bimp$meth

# Set predictors' imp methods
meth[c("A1FULDAY","A1HALFAM","A1HALFPM","A1BOTHCL","A1AHRSDA",
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

# specify predictor matrix
pred <- bimp$predictorMatrix
View(pred)
# Child ID predicts nothing
pred[c(1:83),"CHILDID"] <- 0
# T1MDIFF predicts nothing
pred[c(1:83),"T1MDIFF"] <- 0


# Predictors predicted by nothing 
pred[c("CHILDID","A1FULDAY","A1HALFAM","A1HALFPM","A1BOTHCL","A1AHRSDA",
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
  imp <- mice(may_4_new, maxit=30, m=40, meth=meth, pred=pred, seed=3375)
  end <- Sys.time()
  runtime <- end - start
  runtime 

# summary
# summary(imp6$imp$T1MARSZ)

# Run four more
a <- runif(10)
start <- Sys.time()
imp <- mice.mids(imp, maxit=5)
end <- Sys.time()
runtime <- (end - start)
runtime

# plot
par(mfrow=c(1,2))
plot(imp,c("T1MDIFF", "X1MSCALK1", "X4MSCALK1", "T1MARSAV"))


