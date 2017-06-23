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
### CREATE DECILE VARIABLES####### ###################################################################
######################################################################################################

# gather info
summary(completeimp0519$X1MSCALK1)
# Find decile breaks
quantile(completeimp0519$X1MSCALK1, c(.1, .2, .3, .4, .5, .6, .7, .8, .9))
# Decile breaks: 16.58210 20.84860 23.66760 26.48316 29.48160 32.19022 35.10866 39.58508 45.08063 

# create variable for decile rank
completeimp0519$X1M_decile <- cut(completeimp0519$X1MSCALK1, c(0, 16.5821, 20.84860, 23.66760, 
                                                               26.48316, 29.48160, 32.19022, 
                                                               35.10866, 39.58508, 45.0806, 100))
# Name decile levels
levels(completeimp0519$X1M_decile) <- c("1","2","3","4","5","6","7","8","9","10")
utils::View(completeimp0519)
completeimp0519$X1M_decile <- as.ordered(completeimp0519$X1M_decile)
# summarize 
summary(completeimp0519$X1M_decile)



# gather info
summary(completeimp0519$T1MARSAV)
# Find decile breaks
quantile(completeimp0519$T1MARSAV, c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1))
# Decile breaks: 1.400000 1.750000 2.000000 2.250000 2.571429 2.99 3.166667 3.625000 4.142857 

# create variable for decile rank
completeimp0519$T1M_decile <- cut(completeimp0519$T1MARSAV, c(0, 1.4, 1.75, 2, 2.25, 2.571429, 2.99, 
                                                              3.166667, 3.625000, 4.142857,5))
table(round(completeimp0519$T1MARSAV, 2))
table(completeimp0519$T1M_decile)

# Name decile levels
levels(completeimp0519$T1M_decile) <- c("1","2","3","4","5","6","7","8","9","10")
utils::View(completeimp0519)
completeimp0519$T1M_decile <- as.ordered(completeimp0519$T1M_decile)
# summarize 
summary(completeimp0519$T1M_decile)



# gather info
summary(completeimp0519$T1MDIFF)
# Find decile breaks
quantile(completeimp0519$T1MDIFF, c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1))
# Decile breaks: -3.078 -0.718 -0.335 -0.035  0.221  0.471  0.712  0.966  1.283  1.714  5.089


# create variable for decile rank
completeimp0519$T1D_decile <- cut(completeimp0519$T1MDIFF, c(-3.07765637, -0.71764060, -0.33490648, 
                                                             -0.03484555, 0.22147114, 0.47070940, 
                                                             0.71175584, 0.96551544, 1.28253166, 
                                                             1.71384363, 5.08869665))
# Name decile levels
levels(completeimp0519$T1D_decile) <- c("1","2","3","4","5","6","7","8","9","10")
utils::View(completeimp0519)
completeimp0519$T1D_decile <- as.ordered(completeimp0519$T1D_decile)
# summarize 
summary(completeimp0519$T1D_decile)



######################################################################################################
### PLOT THE PLOTS! ##################################################################################
######################################################################################################

# Does T1MDIFF predict X4MSCALK1? Sorted by decile on X1M
ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1, color=X1M_decile)) +
  coord_cartesian(xlim=c(-3, 5), ylim=c(6,96)) +
  geom_point(alpha=0.2, position=position_jitter()) + stat_smooth(method="lm", se=F) +
  scale_color_manual(name="Decile", values=c("1"="#490092","2"="#006DDB","3"="#B66DFF","4"="#6DB6FF",
                                             "5"="#B6DBFF", "6"="#920000","7"="#924900","8"="#E6AF50",
                                             "9"="#24FF24", "10"="#FFFF6D"), 
                     labels=c("First Decile", "Second Decile","Third Decile","Fourth Decile",
                              "Fifth Decile","Sixth Decile","Seventh Decile","Eighth Decile",
                              "Ninth Decile","Tenth Decile")) 
# Basically no. 

# Does T1MDIFF predict X4MSCALK1? Sorted by decile on T1MARSAV
ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1, color=T1M_decile)) +
  coord_cartesian(xlim=c(-3, 5), ylim=c(6,96)) +
  geom_point(alpha=0.2, position=position_jitter()) + stat_smooth(method="lm", se=F) +
  scale_color_manual(name="Decile", values=c("1"="#490092","2"="#006DDB","3"="#B66DFF","4"="#6DB6FF",
                                             "5"="#B6DBFF", "6"="#920000","7"="#924900","8"="#E6AF50",
                                             "9"="#24FF24", "10"="#FFFF6D"), 
                     labels=c("First Decile", "Second Decile","Third Decile","Fourth Decile",
                              "Fifth Decile","Sixth Decile","Seventh Decile","Eighth Decile",
                              "Ninth Decile","Tenth Decile")) 
# huh.



# ARS predictive of DA?
ggplot(completeimp0519, aes(x=T1MARSAV, y=X4MSCALK1, color=X1M_decile)) +
  coord_cartesian(xlim=c(1, 5), ylim=c(6,96)) +
  geom_point(alpha=0.2, position=position_jitter()) + stat_smooth(method="lm", se=F) +
  
  scale_color_manual(name="Decile", values=c("1"="#490092","2"="#006DDB","3"="#B66DFF","4"="#6DB6FF",
                                             "5"="#B6DBFF", "6"="#920000","7"="#924900","8"="#E6AF50",
                                             "9"="#24FF24", "10"="#FFFF6D"), 
                     labels=c("First Decile", "Second Decile","Third Decile","Fourth Decile",
                              "Fifth Decile","Sixth Decile","Seventh Decile","Eighth Decile",
                              "Ninth Decile","Tenth Decile")) +
  labs(x="T1 ARS Average Score", y="T4 DA Score") +
  labs(title="T4 DA by T1 ARS, grouped by decile on T1 DA")

# Break down the above by sex:
# Males:
ggplot(subset(completeimp0519, X_CHSEX_R=="0"), aes(x=T1MARSAV, y=X4MSCALK1, color=X1M_decile)) +
  coord_cartesian(xlim=c(1, 5), ylim=c(6,96)) +
  geom_point(alpha=0.2, position=position_jitter()) + stat_smooth(method="lm", se=F) +
  
  scale_color_manual(name="Decile", values=c("1"="#490092","2"="#006DDB","3"="#B66DFF","4"="#6DB6FF",
                                             "5"="#B6DBFF", "6"="#920000","7"="#924900","8"="#E6AF50",
                                             "9"="#24FF24", "10"="#FFFF6D"), 
                     labels=c("First Decile", "Second Decile","Third Decile","Fourth Decile",
                              "Fifth Decile","Sixth Decile","Seventh Decile","Eighth Decile",
                              "Ninth Decile","Tenth Decile")) +
  labs(x="T1 ARS Average Score", y="T4 DA Score") +
  labs(title="T4 DA by T1 ARS, grouped by decile on T1 DA")

# Females:
ggplot(subset(completeimp0519, X_CHSEX_R=="1"), aes(x=T1MARSAV, y=X4MSCALK1, color=X1M_decile)) +
  coord_cartesian(xlim=c(1, 5), ylim=c(6,96)) +
  geom_point(alpha=0.2, position=position_jitter()) + stat_smooth(method="lm", se=F) +
  
  scale_color_manual(name="Decile", values=c("1"="#490092","2"="#006DDB","3"="#B66DFF","4"="#6DB6FF",
                                             "5"="#B6DBFF", "6"="#920000","7"="#924900","8"="#E6AF50",
                                             "9"="#24FF24", "10"="#FFFF6D"), 
                     labels=c("First Decile", "Second Decile","Third Decile","Fourth Decile",
                              "Fifth Decile","Sixth Decile","Seventh Decile","Eighth Decile",
                              "Ninth Decile","Tenth Decile")) +
  labs(x="T1 ARS Average Score", y="T4 DA Score") +
  labs(title="T4 DA by T1 ARS, grouped by decile on T1 DA")



# Does the relationship between X1MSCAL and X4MSCAL interact with T1MDIFF or T1MARS?
# T1MARSAV decile
ggplot(completeimp0519, aes(x=X1MSCALK1, y=X4MSCALK1, color=T1M_decile)) +
  coord_cartesian(xlim=c(6,80), ylim=c(6,96)) +
  geom_point(alpha=0.2, position=position_jitter()) + stat_smooth(method="lm", se=F) +
  
  scale_color_manual(name="Decile", values=c("1"="#490092","2"="#006DDB","3"="#B66DFF","4"="#6DB6FF",
                                             "5"="#B6DBFF", "6"="#920000","7"="#924900","8"="#E6AF50",
                                             "9"="#24FF24", "10"="#FFFF6D"), 
                     labels=c("First Decile", "Second Decile","Third Decile","Fourth Decile",
                              "Fifth Decile","Sixth Decile","Seventh Decile","Eighth Decile",
                              "Ninth Decile","Tenth Decile")) +
  labs(x="T1 DA Score", y="T4 DA Score") +
  labs(title="T4 DA by T1 DA, grouped by decile on T1 ARS")

