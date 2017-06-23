
library(mice)
library(ggplot2)
library(graphics)
library(lattice)
library(Hmisc)

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
#levels(may_data$X_CHSEX_R) <- c(0,1)  ?male female??
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
may_six$T1MARSZ <- as.vector(scale(may_six$T1MARSAV, center=T, scale=T))
may_six$T1MDIFF <- may_six$T1MARSZ-may_six$X1MTHETK1
may_six <- may_six[,c(1:5,9:11,13:87)]

utils::View(may_six)
# summary(may_six$T1MARSAV)

may_five$T1MARSAV <- rowMeans(may_five[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH","T1MEASU",
                                          "T1STRAT","T1FRACTN")],na.rm = T)
may_five$T1MARSZ <- scale(may_five$T1MARSAV, center=T, scale=T)
may_five$T1MDIFF <- may_five$T1MARSZ-may_five$X1MTHETK1
# utils::View(may_six)
# summary(may_five$T1MARSAV)

may_four$T1MARSAV <- rowMeans(may_four[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH","T1MEASU",
                                          "T1STRAT","T1FRACTN")],na.rm = T)
may_four$T1MARSZ <- scale(may_four$T1MARSAV, center=T, scale=T)
may_four$T1MDIFF <- may_four$T1MARSZ-may_four$X1MTHETK1
# utils::View(may_four)
# summary(may_four$T1MARSAV)

may_three$T1MARSAV <- rowMeans(may_three[,c("T1SORTS","T1ORDER","T1RELAT","T1SOLVE","T1GRAPH",
                                            "T1MEASU","T1STRAT","T1FRACTN")],na.rm = T)
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


