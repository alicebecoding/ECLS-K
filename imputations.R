





# need to update all this code with proper imp code










######################################################################################################
############ SETTING UP THE IMPUTATION PARAMETERS ####################################################
######################################################################################################

library(mice)
# mice source code URL: http://www.restore.ac.uk/PEAS/ex6datafiles/program_code/micesource.R

# Create the blank imputation form
bimp6 <- mice(may_six, max=0, print=F)
meth6 <- bimp6$meth
meth6

# Set predictors' imp methods
meth6[c("X1TCHEXT","X1TCHAPP","A1FULDAY","A1HALFAM","A1HALFPM","A1BOTHCL","A1AHRSDA",
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
meth6


# put predictor matrix back in mids
bimp6 <- mice(may_six, max=0, meth=meth6, print=F)

# specify predictor matrix
pred6 <- bimp6$predictorMatrix
# View(pred6)

# Sex and race predicted by most
pred6[c("X_CHSEX_R","X_RACETH_R"),] <- 1
# ....except each other
pred6["X_CHSEX_R","X_CHSEX_R"] <- 0
pred6["X_RACETH_R","X_RACETH_R"] <- 0
# T1MARSZ predicted by most
pred6["T1MARSZ",] <- 1
pred6["T1MARSZ","T1MARSZ"] <- 0
# Child ID predicts nothing
pred6[,"CHILDID"] <- 0
# predictors predict everything else
pred6[,c("X1TCHEXT","X1TCHAPP","A1FULDAY","A1HALFAM","A1HALFPM","A1BOTHCL","A1AHRSDA",
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
pred6[c("CHILDID","X1TCHEXT","X1TCHAPP","A1FULDAY","A1HALFAM","A1HALFPM","A1BOTHCL","A1AHRSDA",
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
View(pred6)



######################################################################################################
############ RUNNING THE IMPUTATION ##################################################################
######################################################################################################



# Run one iteration, deliver runtime
start <- Sys.time()
imp6 <- mice(may_six, maxit=1, m=40, meth=meth6, pred=pred6, seed=3375)
end <- Sys.time()
runtime <- end - start
runtime 

# summary
summary(imp6$imp$T1MARSZ)
summary(imp6$imp$T1MDIFF)
summary(imp6$imp$T1MARSAV)

# Run four more
a <- runif(10)
start <- Sys.time()
imp6 <- mice.mids(imp6, maxit=4)
end <- Sys.time()
runtime <- (end - start)
runtime

# plot
plot(imp6, c("X1MTHETK1","T1MARSZ"))

# Total of 10 iterations
a <- runif(10)
start <- Sys.time()
imp6 <- mice.mids(imp6, maxit=5)
end <- Sys.time()
runtime <- (end - start)
runtime

# plot
plot(imp6, c("X1MTHETK1","T1MARSZ"))



######################################################################################################
######################################################################################################
######################################################################################################

