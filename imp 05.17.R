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

######################################################################################################
############ IMPORTING AND CLEANING DATA #############################################################
######################################################################################################


data0517 <- read.csv("Z:/wgehring/Drobo/Alice/ECLS-K/R/2017.05.17.csv")
names(data0517)[1] <- "CHILDID"
# utils::View(data0517)

# recode -9s as NAs
data0517[data0517==-9] <- NA

# replace 6s with NAs
data0517_ARS <- data0517[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH","T1MEASU",
                            "T1STRAT","T1FRACTN","T2SORTS","T2ORDER","T2RELAT","T2SOLVE",
                            "T2GRAPH","T2MEASU","T2STRAT","T2FRACTN","T4PLCVL","T4WHNUM",
                            "T4COINS","T4GRPHAC","T4ESTQNT","T4MEASU","T4STRAT","T4FRACTN")]
data0517_ARS[data0517_ARS==6] <- NA

# Recombine data
data0517 <- cbind(data0517[c("CHILDID","X_CHSEX_R","X1KAGE_R","X_RACETH_R","X4SESL_I","X1TCHCON",
                             "X1TCHEXT","X1TCHAPP","X2TCHCON","X2TCHEXT","X2TCHAPP","X4TCHCON",
                             "X4TCHEXT","X4TCHAPP","X1MSCALK1","X2MSCALK1","X4MSCALK1",
                             "X1MTHETK1","X2MTHETK1","X4MTHETK1")], 
                  data0517_ARS, 
                  data0517[c("A1FULDAY","A1HALFAM","A1HALFPM",
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
                             "A2WRKSMP","A2OBSOBJ","A2MISBHV","A4ENROL","A4JOINE",
                             "A4LEFTL","A4HRSDA","A4DYSWK","A4TOTAG","A4BEHVR",
                             "A4NMELL","A4YRSTCH","A4HIGHQL","A4TARDY","A4ABSEN",
                             "A4BEHVR","A4DISAB","A4DSRV","A4MHLP","A4LUNCH",
                             "A4TPLYIN","A4TPLOUT","A4REGWRK","A4ESLWRK","A4VOLIT",
                             "A4STNTST","A4TSTQZ","A4PROJCT","A4WRKSTS","A4WRKSAM",
                             "A4OBOBJS","A4MISBHV")])
rm(data0517_ARS)

# Change child sex and race to factor variables
data0517$X_CHSEX_R <- as.factor(data0517$X_CHSEX_R)
levels(data0517$X_CHSEX_R) <- c(0,1)
# 0 is male, 1 is female
data0517$X_RACETH_R <- as.factor(data0517$X_RACETH_R)

# Run data through NA cleaning function
data0517 <- na_cleaning_all(data0517, 4, col1.start = 21, col1.end = 28, col2.start=29, 
                            col2.end=36,col3.start=37, col3.end=44)

# Create average ARS scores 
data0517$T1MARSAV <- rowMeans(data0517[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE",
                                          "T1GRAPH","T1MEASU","T1STRAT","T1FRACTN")],na.rm = T)
data0517$T2MARSAV <- rowMeans(data0517[,c("T2SORTS","T2ORDER","T2RELAT","T2SOLVE",
                                          "T2GRAPH","T2MEASU","T2STRAT","T2FRACTN")],na.rm = T)
data0517$T4MARSAV <- rowMeans(data0517[,c("T4PLCVL","T4WHNUM","T4COINS","T4GRPHAC",
                                          "T4ESTQNT","T4MEASU","T4STRAT","T4FRACTN")],na.rm = T)

# Create difference scores
data0517$T1MDIFF <- (as.vector(scale(data0517$T1MARSAV, center=T, scale=T)))-data0517$X1MTHETK1
data0517$T2MDIFF <- (as.vector(scale(data0517$T2MARSAV, center=T, scale=T)))-data0517$X2MTHETK1
data0517$T4MDIFF <- (as.vector(scale(data0517$T4MARSAV, center=T, scale=T)))-data0517$X4MTHETK1
utils::View(data0517)

# Re-organize data for imputation
data0517 <- data0517[,c("CHILDID","X_CHSEX_R","X1KAGE_R","X_RACETH_R","X4SESL_I","X1TCHCON",
                        "X1TCHEXT","X1TCHAPP","X2TCHCON","X2TCHEXT","X2TCHAPP","X4TCHCON",
                        "X4TCHEXT","X4TCHAPP","X1MSCALK1","X2MSCALK1","X4MSCALK1",
                        
                        "T1MARSAV","T2MARSAV","T4MARSAV","T1MDIFF","T2MDIFF","T4MDIFF",
                        
                        "A1FULDAY","A1HALFAM","A1HALFPM","A1BOTHCL","A1AHRSDA","A1PHRSDA",
                        "A1DHRSDA","A1ADYSWK","A1PDYSWK","A1DDYSWK","A1ATOTAG","A1PTOTAG",
                        "A1DTOTAG","A1ABEHVR","A1PBEHVR","A1DBEHVR","A1ANMELL","A1PNMELL",
                        "A1DNMELL","A1CNTRLC","A1CLSSIZ","A1YRSTCH","A1CLSSMN","A1HIGHQL",
                        "A2AENROL","A2PENROL","A2DENROL","A2AJOINE","A2PJOINE","A2DJOINE",
                        "A2ALEFTL","A2PLEFTL","A2DLEFTL","A2ATARDY","A2PTARDY","A2DTARDY",
                        "A2AABSEN","A2PABSEN","A2DABSEN","A2ABEHVR","A2PBEHVR","A2DBEHVR",
                        "A2ASPECN","A2PSPECN","A2DSPECN","A2ASPCIA","A2PSPCIA","A2DSPCIA",
                        "A2AMORE","A2PMORE","A2DMORE","A2LUNCH","A2TPLYIN","A2TPLOUT",
                        "A2REGWRK","A2SPEDWK","A2ESLWRK","A2REGOTH","A2SPEDOT","A2ESLOTH",
                        "A2VOLIT","A2VOLOT","A2STNDRD","A2CMRCLT","A2TCHRMD","A2IGRPRJ",
                        "A2TXTBKT","A2WRKSHT","A2WRKSMP","A2OBSOBJ","A2MISBHV","A4ENROL",
                        "A4JOINE","A4LEFTL","A4HRSDA","A4DYSWK","A4TOTAG","A4BEHVR",
                        "A4NMELL","A4YRSTCH","A4HIGHQL","A4TARDY","A4ABSEN","A4BEHVR",
                        "A4DISAB","A4DSRV","A4MHLP","A4LUNCH","A4TPLYIN","A4TPLOUT",
                        "A4REGWRK","A4ESLWRK","A4VOLIT","A4STNTST","A4TSTQZ","A4PROJCT",
                        "A4WRKSTS","A4WRKSAM","A4OBOBJS","A4MISBHV")]

utils::View(data0517)
# Check a couple things
summary(data0517$T1MDIFF)
summary(data0517$T2MDIFF)
summary(data0517$T4MDIFF)

hist(data0517$X1MSCALK1)
hist(data0517$T1MARSAV)
hist(data0517$X2MSCALK1)
hist(data0517$T2MARSAV)
hist(data0517$X4MSCALK1)
hist(data0517$T4MARSAV)



######################################################################################################
############ SETTING UP THE IMPUTATION PARAMETERS ####################################################
######################################################################################################



# library(mice)
# mice source code URL: http://www.restore.ac.uk/PEAS/ex6datafiles/program_code/micesource.R

# Create the blank imputation form
bimp0517 <- mice(data0517, max=0, print=F)
meth0517 <- bimp0517$meth

# Set predictors' imp methods
meth0517[c("CHILDID","A1FULDAY","A1HALFAM","A1HALFPM","A1BOTHCL","A1AHRSDA","A1PHRSDA",
           "A1DHRSDA","A1ADYSWK","A1PDYSWK","A1DDYSWK","A1ATOTAG","A1PTOTAG",
           "A1DTOTAG","A1ABEHVR","A1PBEHVR","A1DBEHVR","A1ANMELL","A1PNMELL",
           "A1DNMELL","A1CNTRLC","A1CLSSIZ","A1YRSTCH","A1CLSSMN","A1HIGHQL",
           "A2AENROL","A2PENROL","A2DENROL","A2AJOINE","A2PJOINE","A2DJOINE",
           "A2ALEFTL","A2PLEFTL","A2DLEFTL","A2ATARDY","A2PTARDY","A2DTARDY",
           "A2AABSEN","A2PABSEN","A2DABSEN","A2ABEHVR","A2PBEHVR","A2DBEHVR",
           "A2ASPECN","A2PSPECN","A2DSPECN","A2ASPCIA","A2PSPCIA","A2DSPCIA",
           "A2AMORE","A2PMORE","A2DMORE","A2LUNCH","A2TPLYIN","A2TPLOUT",
           "A2REGWRK","A2SPEDWK","A2ESLWRK","A2REGOTH","A2SPEDOT","A2ESLOTH",
           "A2VOLIT","A2VOLOT","A2STNDRD","A2CMRCLT","A2TCHRMD","A2IGRPRJ",
           "A2TXTBKT","A2WRKSHT","A2WRKSMP","A2OBSOBJ","A2MISBHV","A4ENROL",
           "A4JOINE","A4LEFTL","A4HRSDA","A4DYSWK","A4TOTAG","A4BEHVR",
           "A4NMELL","A4YRSTCH","A4HIGHQL","A4TARDY","A4ABSEN","A4BEHVR",
           "A4DISAB","A4DSRV","A4MHLP","A4LUNCH","A4TPLYIN","A4TPLOUT",
           "A4REGWRK","A4ESLWRK","A4VOLIT","A4STNTST","A4TSTQZ","A4PROJCT",
           "A4WRKSTS","A4WRKSAM","A4OBOBJS","A4MISBHV")] <- ""
meth0517

# specify predictor matrix
pred0517 <- bimp0517$predictorMatrix
utils::View(pred0517)
# Child ID predicts nothing
pred0517[c(1:122),"CHILDID"] <- 0
# MDIFF predicts nothing
pred0517[c(1:122),c("T1MDIFF","T2MDIFF","T4MDIFF")] <- 0
utils::View(pred0517)


# Predictors predicted by nothing 
pred0517[c("CHILDID","A1FULDAY","A1HALFAM","A1HALFPM","A1BOTHCL","A1AHRSDA",
           "A1PHRSDA","A1DHRSDA","A1ADYSWK","A1PDYSWK","A1DDYSWK","A1ATOTAG",
           "A1PTOTAG","A1DTOTAG","A1ABEHVR","A1PBEHVR","A1DBEHVR","A1ANMELL",
           "A1PNMELL","A1DNMELL","A1CNTRLC","A1CLSSIZ","A1YRSTCH","A1CLSSMN",
           "A1HIGHQL","A2AENROL","A2PENROL","A2DENROL","A2AJOINE","A2PJOINE",
           "A2DJOINE","A2ALEFTL","A2PLEFTL","A2DLEFTL","A2ATARDY","A2PTARDY",
           "A2DTARDY","A2AABSEN","A2PABSEN","A2DABSEN","A2ABEHVR","A2PBEHVR",
           "A2DBEHVR","A2ASPECN","A2PSPECN","A2DSPECN","A2ASPCIA","A2PSPCIA",
           "A2DSPCIA","A2AMORE","A2PMORE","A2DMORE","A2LUNCH","A2TPLYIN",
           "A2TPLOUT","A2REGWRK","A2SPEDWK","A2ESLWRK","A2REGOTH","A2SPEDOT",
           "A2ESLOTH","A2VOLIT","A2VOLOT","A2STNDRD","A2CMRCLT","A2TCHRMD",
           "A2IGRPRJ","A2TXTBKT","A2WRKSHT","A2WRKSMP","A2OBSOBJ","A2MISBHV",
           "A4BEHVR","A4NMELL","A4YRSTCH","A4HIGHQL","A4TARDY","A4ABSEN",
           "A4BEHVR","A4DISAB","A4DSRV","A4MHLP","A4LUNCH","A4TPLYIN",
           "A4TPLOUT","A4REGWRK","A4ESLWRK","A4VOLIT","A4STNTST","A4TSTQZ",
           "A4PROJCT","A4WRKSTS","A4WRKSAM","A4OBOBJS","A4MISBHV"),] <- 0
utils::View(pred0517)

######################################################################################################
############ RUNNING THE IMPUTATION ##################################################################
######################################################################################################



# Run one iteration, deliver runtime
#start <- Sys.time()
imp0517 <- mice(data0517, maxit=1, m=40, meth=meth0517, pred=pred0517, seed=3375)
#end <- Sys.time()
#runtime <- end - start
#runtime 

# Run 29 more more
a <- runif(10)
start <- Sys.time()
imp <- mice.mids(imp0517, maxit=29)
end <- Sys.time()
runtime <- (end - start)
runtime

# plot
plot(imp0517,c("T1MDIFF", "X1MSCALK1", "X4MSCALK1", "T1MARSAV"))
summary(imp0517$imp$X1TCHCON)


