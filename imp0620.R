# K-2 File

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

getwd()
setwd("Z:/wgehring/Drobo/Alice/ECLS-K/Data")
data0620 <- read.csv("ECLSK2_2017.06.20.csv")
utils::View(data0620)

# replace 6s with NAs
data0620_ARS <- data0620[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH","T1MEASU",
                            "T1STRAT","T1FRACTN","T2SORTS","T2ORDER","T2RELAT","T2SOLVE",
                            "T2GRAPH","T2MEASU","T2STRAT","T2FRACTN","T4PLCVL","T4WHNUM",
                            "T4COINS","T4GRPHAC","T4ESTQNT","T4MEASU","T4STRAT","T4FRACTN")]
data0620_ARS[data0620_ARS==6] <- NA
View(data0620_ARS)

data0620 <- cbind(data0620[,c("CHILDID",	"X_CHSEX_R",	"X1KAGE_R",	"X2KAGE_R",	"X3AGE",	"X4AGE",	
                              "X5AGE","X6AGE",	"X_RACETH_R",	"X4SESL_I",	"X1TCHCON",	"X2TCHCON",	
                              "X3TCHCON", "X4TCHCON",	"X5TCHCON",	"X6TCHCON",	"X1TCHPER",	"X2TCHPER", 
                              "X3TCHPER", "X4TCHPER",	"X5TCHPER",	"X6TCHPER",	"X1TCHEXT",	"X2TCHEXT",	
                              "X3TCHEXT",	"X4TCHEXT",	"X5TCHEXT",	"X6TCHEXT",	"X1TCHINT",	"X2TCHINT",	
                              "X3TCHINT",	"X4TCHINT",	"X5TCHINT",	"X6TCHINT",	"X1TCHAPP",	"X2TCHAPP",	
                              "X3TCHAPP",	"X4TCHAPP",	"X5TCHAPP",	"X6TCHAPP",	"X1ATTNFS",	"X2ATTNFS",	
                              "X4ATTNFS",	"X6ATTMCQ",	"X1INBCNT",	"X2INBCNT",	"X4INBCNT",	"X6INTMCQ",	
                              "X2CLSNSS",	"X4CLSNSS",	"X6CLSNSS",	"X2CNFLCT",	"X4CNFLCT",	"X6CNFLCT",	
                              "X1MSCALK2","X2MSCALK2",	"X3MSCALK2",	"X4MSCALK2",	"X5MSCALK2",
                              "X6MSCALK2",	"X1MTHETK2",	"X2MTHETK2",	"X3MTHETK2",	"X4MTHETK2",	
                              "X5MTHETK2",	"X6MTHETK2")], 
                  data0620_ARS, 
                  data0620[,c("A1FULDAY",	"A2FULDAY",	"A1HALFAM",	"A2HALFAM",	"A1HALFPM",	
                              "A2HALFPM",	"A1AHRSDA",	"A1PHRSDA",	"A1DHRSDA",	"A4HRSDA",	"A6HRSDA",	
                              "A1ADYSWK",	"A1PDYSWK",	"A1DDYSWK",	"A4DYSWK",	"A6DYSWK",	"A1ATOTAG",	
                              "A1PTOTAG",	"A1DTOTAG",	"A4TOTAG",	"A6TOTAG",	"A2AENROL",	"A2PENROL",	
                              "A2DENROL",	"A4ENROL",	"A6ENROL",	"A2AJOINE",	"A2PJOINE",	"A2DJOINE",	
                              "A4JOINE",	"A6JOINE",	"A2ALEFTL",	"A2PLEFTL",	"A2DLEFTL",	"A4LEFTL",	
                              "A6LEFTL",	"A2ATARDY",	"A2PTARDY",	"A2DTARDY",	"A4TARDY",	"A6TARDY",	
                              "A2AABSEN",	"A2PABSEN",	"A2DABSEN",	"A4ABSEN",	"A6ABSEN",	"A1ABEHVR",	
                              "A1PBEHVR",	"A1DBEHVR",	"A2ABEHVR",	"A2PBEHVR",	"A2DBEHVR",	"A4BEHVR",	
                              "A6BEHVR",	"A1ANMELL",	"A1PNMELL",	"A1DNMELL",	"A4NMELL",	"A6NMELL",	
                              "A1CNTRLC",	"A1CLSSIZ",	"A1YRSTCH",	"A4YRSTCH",	"A6YRSTCH",	"A1CLSSMN",	
                              "B2CLSSMN",	"A1HIGHQL",	"B2HIGHQL",	"A4HIGHQL",	"A6HIGHQL",	"A2ASPECN",	
                              "A2PSPECN",	"A2DSPECN",	"A4DISAB",	"A6DISAB",	"A2ASPCIA",	"A2PSPCIA",	
                              "A2DSPCIA",	"A4DSRV",	"A6DSRV",	"A2AMORE",	"A2PMORE",	"A2DMORE",	
                              "A4MHLP",	"A6MHLP",	"A2LUNCH",	"A4LUNCH",	"A6LUNCH",	"A2TPLYIN",	
                              "A4TPLYIN",	"A6TPLYIN",	"A2TPLOUT",	"A4TPLOUT",	"A6TPLOUT",	"A2REGWRK",	
                              "A4REGWRK",	"A6REGWRK",	"A2SPEDWK",	"A4SPEDWK",	"A6SPEDWK",	"A2ESLWRK",	
                              "A4ESLWRK",	"A6ESLWRK",	"A2REGOTH",	"A2SPEDOT",	"A2ESLOTH",	"A2VOLIT",	
                              "A4VOLIT",	"A6VOLIT",	"A2VOLOT",	"A2STNDRD",	"A4STNTST",	"A6STNTST",	
                              "A2CMRCLT",	"A2TCHRMD",	"A4TSTQZ",	"A6TSTQZ",	"A2IGRPRJ",	"A4PROJCT",	
                              "A6PROJCT",	"A2TXTBKT",	"A2WRKSHT",	"A4WRKSTS",	"A6WRKSTS",	"A2WRKSMP",	
                              "A4WRKSAM",	"A6WRKSAM",	"A2OBSOBJ",	"A4OBOBJS",	"A6OBOBJS",	"A2MISBHV",	
                              "A4MISBHV",	"A6MISBHV")])
# recode -9s as NAs
data0620[data0620==-9] <- NA

# rm ARS
rm(data0620_ARS)

# Change child sex and race to factor variables
data0620$X_CHSEX_R <- as.factor(data0620$X_CHSEX_R)
levels(data0620$X_CHSEX_R) <- c(0,1)
# 0 is male, 1 is female
data0620$X_RACETH_R <- as.factor(data0620$X_RACETH_R)

# Run data through NA cleaning function
data0620 <- na_cleaning_all(data0620, 4, col1.start = 67, col1.end = 74, col2.start=75, 
                            col2.end=82,col3.start=83, col3.end=90)

# Create average ARS scores 
data0620$T1MARSAV <- rowMeans(data0620[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE",
                                          "T1GRAPH","T1MEASU","T1STRAT","T1FRACTN")],na.rm = T)
data0620$T2MARSAV <- rowMeans(data0620[,c("T2SORTS","T2ORDER","T2RELAT","T2SOLVE",
                                          "T2GRAPH","T2MEASU","T2STRAT","T2FRACTN")],na.rm = T)
data0620$T4MARSAV <- rowMeans(data0620[,c("T4PLCVL","T4WHNUM","T4COINS","T4GRPHAC",
                                          "T4ESTQNT","T4MEASU","T4STRAT","T4FRACTN")],na.rm = T)

# Create difference scores
data0620$T1MDIFF <- (as.vector(scale(data0620$T1MARSAV, center=T, scale=T)))-data0620$X1MTHETK2
data0620$T2MDIFF <- (as.vector(scale(data0620$T2MARSAV, center=T, scale=T)))-data0620$X2MTHETK2
data0620$T4MDIFF <- (as.vector(scale(data0620$T4MARSAV, center=T, scale=T)))-data0620$X4MTHETK2
utils::View(data0620)

# Re-organize data for imputation
data0620 <- data0620[,c("CHILDID",	"X_CHSEX_R",	"X1KAGE_R",	"X2KAGE_R",	"X3AGE",	"X4AGE",	
                        "X5AGE","X6AGE",	"X_RACETH_R",	"X4SESL_I",	"X1TCHCON",	"X2TCHCON",	
                        "X3TCHCON", "X4TCHCON",	"X5TCHCON",	"X6TCHCON",	"X1TCHPER",	"X2TCHPER", 
                        "X3TCHPER", "X4TCHPER",	"X5TCHPER",	"X6TCHPER",	"X1TCHEXT",	"X2TCHEXT",	
                        "X3TCHEXT",	"X4TCHEXT",	"X5TCHEXT",	"X6TCHEXT",	"X1TCHINT",	"X2TCHINT",	
                        "X3TCHINT",	"X4TCHINT",	"X5TCHINT",	"X6TCHINT",	"X1TCHAPP",	"X2TCHAPP",	
                        "X3TCHAPP",	"X4TCHAPP",	"X5TCHAPP",	"X6TCHAPP",	"X1ATTNFS",	"X2ATTNFS",	
                        "X4ATTNFS",	"X6ATTMCQ",	"X1INBCNT",	"X2INBCNT",	"X4INBCNT",	"X6INTMCQ",	
                        "X2CLSNSS",	"X4CLSNSS",	"X6CLSNSS",	"X2CNFLCT",	"X4CNFLCT",	"X6CNFLCT",	
                        "X1MSCALK2","X2MSCALK2",	"X3MSCALK2",	"X4MSCALK2",	"X5MSCALK2",
                        "X6MSCALK2",	"X1MTHETK2",	"X2MTHETK2",	"X3MTHETK2",	"X4MTHETK2",	
                        "X5MTHETK2",	"X6MTHETK2",
                        
                        "T1MARSAV","T2MARSAV","T4MARSAV","T1MDIFF","T2MDIFF","T4MDIFF",
                        
                        "A1FULDAY",	"A2FULDAY",	"A1HALFAM",	"A2HALFAM",	"A1HALFPM",	
                        "A2HALFPM",	"A1AHRSDA",	"A1PHRSDA",	"A1DHRSDA",	"A4HRSDA",	"A6HRSDA",	
                        "A1ADYSWK",	"A1PDYSWK",	"A1DDYSWK",	"A4DYSWK",	"A6DYSWK",	"A1ATOTAG",	
                        "A1PTOTAG",	"A1DTOTAG",	"A4TOTAG",	"A6TOTAG",	"A2AENROL",	"A2PENROL",	
                        "A2DENROL",	"A4ENROL",	"A6ENROL",	"A2AJOINE",	"A2PJOINE",	"A2DJOINE",	
                        "A4JOINE",	"A6JOINE",	"A2ALEFTL",	"A2PLEFTL",	"A2DLEFTL",	"A4LEFTL",	
                        "A6LEFTL",	"A2ATARDY",	"A2PTARDY",	"A2DTARDY",	"A4TARDY",	"A6TARDY",	
                        "A2AABSEN",	"A2PABSEN",	"A2DABSEN",	"A4ABSEN",	"A6ABSEN",	"A1ABEHVR",	
                        "A1PBEHVR",	"A1DBEHVR",	"A2ABEHVR",	"A2PBEHVR",	"A2DBEHVR",	"A4BEHVR",	
                        "A6BEHVR",	"A1ANMELL",	"A1PNMELL",	"A1DNMELL",	"A4NMELL",	"A6NMELL",	
                        "A1CNTRLC",	"A1CLSSIZ",	"A1YRSTCH",	"A4YRSTCH",	"A6YRSTCH",	"A1CLSSMN",	
                        "B2CLSSMN",	"A1HIGHQL",	"B2HIGHQL",	"A4HIGHQL",	"A6HIGHQL",	"A2ASPECN",	
                        "A2PSPECN",	"A2DSPECN",	"A4DISAB",	"A6DISAB",	"A2ASPCIA",	"A2PSPCIA",	
                        "A2DSPCIA",	"A4DSRV",	"A6DSRV",	"A2AMORE",	"A2PMORE",	"A2DMORE",	
                        "A4MHLP",	"A6MHLP",	"A2LUNCH",	"A4LUNCH",	"A6LUNCH",	"A2TPLYIN",	
                        "A4TPLYIN",	"A6TPLYIN",	"A2TPLOUT",	"A4TPLOUT",	"A6TPLOUT",	"A2REGWRK",	
                        "A4REGWRK",	"A6REGWRK",	"A2SPEDWK",	"A4SPEDWK",	"A6SPEDWK",	"A2ESLWRK",	
                        "A4ESLWRK",	"A6ESLWRK",	"A2REGOTH",	"A2SPEDOT",	"A2ESLOTH",	"A2VOLIT",	
                        "A4VOLIT",	"A6VOLIT",	"A2VOLOT",	"A2STNDRD",	"A4STNTST",	"A6STNTST",	
                        "A2CMRCLT",	"A2TCHRMD",	"A4TSTQZ",	"A6TSTQZ",	"A2IGRPRJ",	"A4PROJCT",	
                        "A6PROJCT",	"A2TXTBKT",	"A2WRKSHT",	"A4WRKSTS",	"A6WRKSTS",	"A2WRKSMP",	
                        "A4WRKSAM",	"A6WRKSAM",	"A2OBSOBJ",	"A4OBOBJS",	"A6OBOBJS",	"A2MISBHV",	
                        "A4MISBHV",	"A6MISBHV")]



######################################################################################################
############ SETTING UP THE IMPUTATION PARAMETERS ####################################################
######################################################################################################

# mice source code URL: http://www.restore.ac.uk/PEAS/ex6datafiles/program_code/micesource.R

# Create the blank imputation form
bimp0620 <- mice(data0620, max=0, print=F)
meth0620 <- bimp0620$meth

# Set predictors' imp methods
meth0620[c("CHILDID","A1FULDAY",	"A2FULDAY",	"A1HALFAM",	"A2HALFAM",	"A1HALFPM",	
           "A2HALFPM",	"A1AHRSDA",	"A1PHRSDA",	"A1DHRSDA",	"A4HRSDA",	"A6HRSDA",	
           "A1ADYSWK",	"A1PDYSWK",	"A1DDYSWK",	"A4DYSWK",	"A6DYSWK",	"A1ATOTAG",	
           "A1PTOTAG",	"A1DTOTAG",	"A4TOTAG",	"A6TOTAG",	"A2AENROL",	"A2PENROL",	
           "A2DENROL",	"A4ENROL",	"A6ENROL",	"A2AJOINE",	"A2PJOINE",	"A2DJOINE",	
           "A4JOINE",	"A6JOINE",	"A2ALEFTL",	"A2PLEFTL",	"A2DLEFTL",	"A4LEFTL",	
           "A6LEFTL",	"A2ATARDY",	"A2PTARDY",	"A2DTARDY",	"A4TARDY",	"A6TARDY",	
           "A2AABSEN",	"A2PABSEN",	"A2DABSEN",	"A4ABSEN",	"A6ABSEN",	"A1ABEHVR",	
           "A1PBEHVR",	"A1DBEHVR",	"A2ABEHVR",	"A2PBEHVR",	"A2DBEHVR",	"A4BEHVR",	
           "A6BEHVR",	"A1ANMELL",	"A1PNMELL",	"A1DNMELL",	"A4NMELL",	"A6NMELL",	
           "A1CNTRLC",	"A1CLSSIZ",	"A1YRSTCH",	"A4YRSTCH",	"A6YRSTCH",	"A1CLSSMN",	
           "B2CLSSMN",	"A1HIGHQL",	"B2HIGHQL",	"A4HIGHQL",	"A6HIGHQL",	"A2ASPECN",	
           "A2PSPECN",	"A2DSPECN",	"A4DISAB",	"A6DISAB",	"A2ASPCIA",	"A2PSPCIA",	
           "A2DSPCIA",	"A4DSRV",	"A6DSRV",	"A2AMORE",	"A2PMORE",	"A2DMORE",	
           "A4MHLP",	"A6MHLP",	"A2LUNCH",	"A4LUNCH",	"A6LUNCH",	"A2TPLYIN",	
           "A4TPLYIN",	"A6TPLYIN",	"A2TPLOUT",	"A4TPLOUT",	"A6TPLOUT",	"A2REGWRK",	
           "A4REGWRK",	"A6REGWRK",	"A2SPEDWK",	"A4SPEDWK",	"A6SPEDWK",	"A2ESLWRK",	
           "A4ESLWRK",	"A6ESLWRK",	"A2REGOTH",	"A2SPEDOT",	"A2ESLOTH",	"A2VOLIT",	
           "A4VOLIT",	"A6VOLIT",	"A2VOLOT",	"A2STNDRD",	"A4STNTST",	"A6STNTST",	
           "A2CMRCLT",	"A2TCHRMD",	"A4TSTQZ",	"A6TSTQZ",	"A2IGRPRJ",	"A4PROJCT",	
           "A6PROJCT",	"A2TXTBKT",	"A2WRKSHT",	"A4WRKSTS",	"A6WRKSTS",	"A2WRKSMP",	
           "A4WRKSAM",	"A6WRKSAM",	"A2OBSOBJ",	"A4OBOBJS",	"A6OBOBJS",	"A2MISBHV",	
           "A4MISBHV",	"A6MISBHV")] <- ""
meth0620

# specify predictor matrix
pred0620 <- bimp0620$predictorMatrix
utils::View(pred0620)
# Child ID predicts nothing
pred0620[c(1:204),"CHILDID"] <- 0
# MDIFF predicts nothing
pred0620[c(1:204),c("T1MDIFF","T2MDIFF","T4MDIFF")] <- 0
# Thetas predict nothing
pred0620[c(1:204,"X1MTHETK2",	"X2MTHETK2",	"X3MTHETK2",	"X4MTHETK2", "X5MTHETK2",	"X6MTHETK2")]
utils::View(pred0620)


# Predictors predicted by nothing 
pred0620[c("CHILDID","A1FULDAY",	"A2FULDAY",	"A1HALFAM",	"A2HALFAM",	"A1HALFPM",	
           "A2HALFPM",	"A1AHRSDA",	"A1PHRSDA",	"A1DHRSDA",	"A4HRSDA",	"A6HRSDA",	
           "A1ADYSWK",	"A1PDYSWK",	"A1DDYSWK",	"A4DYSWK",	"A6DYSWK",	"A1ATOTAG",	
           "A1PTOTAG",	"A1DTOTAG",	"A4TOTAG",	"A6TOTAG",	"A2AENROL",	"A2PENROL",	
           "A2DENROL",	"A4ENROL",	"A6ENROL",	"A2AJOINE",	"A2PJOINE",	"A2DJOINE",	
           "A4JOINE",	"A6JOINE",	"A2ALEFTL",	"A2PLEFTL",	"A2DLEFTL",	"A4LEFTL",	
           "A6LEFTL",	"A2ATARDY",	"A2PTARDY",	"A2DTARDY",	"A4TARDY",	"A6TARDY",	
           "A2AABSEN",	"A2PABSEN",	"A2DABSEN",	"A4ABSEN",	"A6ABSEN",	"A1ABEHVR",	
           "A1PBEHVR",	"A1DBEHVR",	"A2ABEHVR",	"A2PBEHVR",	"A2DBEHVR",	"A4BEHVR",	
           "A6BEHVR",	"A1ANMELL",	"A1PNMELL",	"A1DNMELL",	"A4NMELL",	"A6NMELL",	
           "A1CNTRLC",	"A1CLSSIZ",	"A1YRSTCH",	"A4YRSTCH",	"A6YRSTCH",	"A1CLSSMN",	
           "B2CLSSMN",	"A1HIGHQL",	"B2HIGHQL",	"A4HIGHQL",	"A6HIGHQL",	"A2ASPECN",	
           "A2PSPECN",	"A2DSPECN",	"A4DISAB",	"A6DISAB",	"A2ASPCIA",	"A2PSPCIA",	
           "A2DSPCIA",	"A4DSRV",	"A6DSRV",	"A2AMORE",	"A2PMORE",	"A2DMORE",	
           "A4MHLP",	"A6MHLP",	"A2LUNCH",	"A4LUNCH",	"A6LUNCH",	"A2TPLYIN",	
           "A4TPLYIN",	"A6TPLYIN",	"A2TPLOUT",	"A4TPLOUT",	"A6TPLOUT",	"A2REGWRK",	
           "A4REGWRK",	"A6REGWRK",	"A2SPEDWK",	"A4SPEDWK",	"A6SPEDWK",	"A2ESLWRK",	
           "A4ESLWRK",	"A6ESLWRK",	"A2REGOTH",	"A2SPEDOT",	"A2ESLOTH",	"A2VOLIT",	
           "A4VOLIT",	"A6VOLIT",	"A2VOLOT",	"A2STNDRD",	"A4STNTST",	"A6STNTST",	
           "A2CMRCLT",	"A2TCHRMD",	"A4TSTQZ",	"A6TSTQZ",	"A2IGRPRJ",	"A4PROJCT",	
           "A6PROJCT",	"A2TXTBKT",	"A2WRKSHT",	"A4WRKSTS",	"A6WRKSTS",	"A2WRKSMP",	
           "A4WRKSAM",	"A6WRKSAM",	"A2OBSOBJ",	"A4OBOBJS",	"A6OBOBJS",	"A2MISBHV",	
           "A4MISBHV",	"A6MISBHV"),] <- 0
utils::View(pred0620)



######################################################################################################
############ RUNNING THE IMPUTATION ##################################################################
######################################################################################################

# Run one iteration, deliver runtime
start <- Sys.time()
imp0620 <- mice(data0620, maxit=1, m=40, meth=meth0620, pred=pred0620, seed=3375)
end <- Sys.time()
runtime <- end - start
runtime 

# Run 19 more 
a <- runif(10)
start <- Sys.time()
imp <- mice.mids(imp0620, maxit=19)
end <- Sys.time()
runtime <- (end - start)
runtime

# plot
plot(imp0517,c("T1MDIFF", "X1MSCALK1", "X4MSCALK1", "T1MARSAV"))
summary(imp0517$imp$X1TCHCON)





