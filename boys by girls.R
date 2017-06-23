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


# X4 by X1
ggplot(completeimp0519, aes(x=X1MSCALK1, y=X4MSCALK1)) +
  geom_point(data=subset(completeimp0519, X_CHSEX_R=="0"), aes(color="Male"), alpha=0.2)  +
  geom_point(data=subset(completeimp0519, X_CHSEX_R=="1"), aes(color="Female"), alpha=0.2) +
  geom_smooth(method=lm, se=F, data=subset(completeimp0519, X_CHSEX_R=="0"), aes(color="Male")) +
  geom_smooth(method=lm, se=F, data=subset(completeimp0519, X_CHSEX_R=="1"), aes(color="Female")) +
  scale_color_manual(name="Sex", values=c("steelblue","gold"), labels=c("Male","Female")) +
  coord_cartesian(xlim=c(0, 80), ylim=c(12,100)) 

# T4 by X1
ggplot(completeimp0519, aes(x=X1MSCALK1, y=T4MARSAV)) +
  geom_point(data=subset(completeimp0519, X_CHSEX_R=="0"), aes(color="Male"), alpha=0.2)  +
  geom_point(data=subset(completeimp0519, X_CHSEX_R=="1"), aes(color="Female"), alpha=0.2) +
  geom_smooth(method=lm, se=F, data=subset(completeimp0519, X_CHSEX_R=="0"), aes(color="Male")) +
  geom_smooth(method=lm, se=F, data=subset(completeimp0519, X_CHSEX_R=="1"), aes(color="Female")) +
  scale_color_manual(name="Sex", values=c("steelblue","gold"), labels=c("Male","Female")) +
  coord_cartesian(xlim=c(1, 80), ylim=c(1,5)) 

# T4 by T1
ggplot(completeimp0519, aes(x=T1MARSAV, y=T4MARSAV)) +
  geom_point(data=subset(completeimp0519, X_CHSEX_R=="0"), aes(color="Male"), alpha=0.2)  +
  geom_point(data=subset(completeimp0519, X_CHSEX_R=="1"), aes(color="Female"), alpha=0.2) +
  geom_smooth(method=lm, se=F, data=subset(completeimp0519, X_CHSEX_R=="0"), aes(color="Male")) +
  geom_smooth(method=lm, se=F, data=subset(completeimp0519, X_CHSEX_R=="1"), aes(color="Female")) +
  scale_color_manual(name="Sex", values=c("steelblue","gold"), labels=c("Male","Female")) 

# X4 by T1
ggplot(completeimp0519, aes(x=T1MARSAV, y=X4MSCALK1)) +
  geom_point(data=subset(completeimp0519, X_CHSEX_R=="0"), aes(color="Male"), alpha=0.2)  +
  geom_point(data=subset(completeimp0519, X_CHSEX_R=="1"), aes(color="Female"), alpha=0.2) +
  geom_smooth(method=lm, se=F, data=subset(completeimp0519, X_CHSEX_R=="0"), aes(color="Male")) +
  geom_smooth(method=lm, se=F, data=subset(completeimp0519, X_CHSEX_R=="1"), aes(color="Female")) +
  scale_color_manual(name="Sex", values=c("steelblue","gold"), labels=c("Male","Female")) +
  coord_cartesian(xlim=c(1,5), ylim=c(10, 100)) 

## X4 by T1Diff
#ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1)) +
#  geom_point(data=subset(completeimp0519, X_CHSEX_R=="0"), aes(color="Male"), alpha=0.2)  +
#  geom_point(data=subset(completeimp0519, X_CHSEX_R=="1"), aes(color="Female"), alpha=0.2) +
#  geom_smooth(method=lm, se=F, data=subset(completeimp0519, X_CHSEX_R=="0"), aes(color="Male")) +
#  geom_smooth(method=lm, se=F, data=subset(completeimp0519, X_CHSEX_R=="1"), aes(color="Female")) +
#  scale_color_manual(name="Sex", values=c("steelblue","gold"), labels=c("Male","Female"))  

## T4 by T1Diff
#ggplot(completeimp0519, aes(x=T1MDIFF, y=T4MARSAV)) +
#  geom_point(data=subset(completeimp0519, X_CHSEX_R=="0"), aes(color="Male"), alpha=0.2)  +
#  geom_point(data=subset(completeimp0519, X_CHSEX_R=="1"), aes(color="Female"), alpha=0.2) +
#  geom_smooth(method=lm, se=F, data=subset(completeimp0519, X_CHSEX_R=="0"), aes(color="Male")) +
#  geom_smooth(method=lm, se=F, data=subset(completeimp0519, X_CHSEX_R=="1"), aes(color="Female")) +
#  scale_color_manual(name="Sex", values=c("steelblue","gold"), labels=c("Male","Female"))  

summary(lm(X4MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=boysimp))
lm.beta(lm(X4MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=boysimp))

summary(lm(X2MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=boysimp))
lm.beta(lm(X2MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=boysimp))

summary(lm(X4MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=girlsimp))
lm.beta(lm(X4MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=girlsimp))

summary(lm(X2MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=girlsimp))
lm.beta(lm(X2MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=girlsimp))


# Top and bottom decile for girls
ggplot(completeimp0519, aes(x=X1MSCALK1, y=X2MSCALK1))  + 
  coord_cartesian(xlim=c(6,80), ylim=c(6, 80)) +
  geom_point(data=subset(completeimp0519, X1M_decile=="1"&X_CHSEX_R=="1"), aes(color="490092"), 
             alpha=0.2) +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519, X1M_decile=="1"&X_CHSEX_R=="1"), 
              aes(color="490092")) + 
  geom_point(data=subset(completeimp0519, X1M_decile=="10"&X_CHSEX_R=="1"), aes(color="FFFF6D"), 
             alpha=0.2) +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519, X1M_decile=="10"&X_CHSEX_R=="1"), 
              aes(color="FFFF6D")) + 
  scale_color_manual(name="Decile", values=c("#490092", "#FFFF6D"), 
                     labels=c("First Decile", "Tenth Decile")) 
  
# Top and bottom quartile for girls

# quantile(completeimp0519$X1MSCALK1, c(.25, .5, .75))
# completeimp0519$X1M_quartile <- cut(completeimp0519$X1MSCALK1, c(0, 22.29037, 29.48160, 37.12750, 100))
# levels(completeimp0519$X1M_quartile) <- c("1", "2", "3", "4")
# summary(completeimp0519$X1M_quartile)


ggplot(completeimp0519, aes(x=X1MSCALK1, y=X4MSCALK1))  + 
  coord_cartesian(xlim=c(6,80), ylim=c(6,96)) +
  geom_point(data=subset(completeimp0519, X1M_quartile=="1"&X_CHSEX_R=="1"), aes(color="490092"), alpha=0.2) +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519, X1M_quartile=="1"&X_CHSEX_R=="1"), 
              aes(color="490092")) + 
  geom_smooth(method="lm", se=F, data=subset(completeimp0519, X1M_quartile=="4"&X_CHSEX_R=="1"), 
              aes(color="FFFF6D")) + 
  geom_point(data=subset(completeimp0519, X1M_quartile=="4"&X_CHSEX_R=="1"), aes(color="FFFF6D"), alpha=0.2) +
  scale_color_manual(name="Quartile", values=c("#490092","#FFFF6D"), labels=c("First Quartile","Fourth Quartile"))

# More
# girlsimp <- subset(completeimp0519, X_CHSEX_R==1)
# boysimp <- subset(completeimp0519, X_CHSEX_R==0)

summary(lm(X4MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=boysimp))
lm.beta(lm(X4MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=boysimp))

summary(lm(X2MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=boysimp))
lm.beta(lm(X2MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=boysimp))

summary(lm(X4MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=girlsimp))
lm.beta(lm(X4MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=girlsimp))

summary(lm(X2MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=girlsimp))
lm.beta(lm(X2MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=girlsimp))


fit_T1_X2_B <- lm(X2MSCALK1~X1MSCALK1, data=boysimp)
ggplotRegression(fit_T1_X2_B)
lm.beta(fit_T1_X2_B)

fit_T1_X2_G <- lm(X2MSCALK1~X1MSCALK1, data=girlsimp)
ggplotRegression(fit_T1_X2_G)
lm.beta(fit_T1_X2_G)

fit_behav_boys <- lm(X4MSCALK1 ~ X1TCHAPP, data=boysimp) 
summary(fit_behav_boys)
lm.beta(fit_behav_boys)
ggplotRegression(fit_behav_boys)
# TCHCON: std. beta of 0.24
# TCHAPP: std. beta of 0.42
# TCHEXT: std. beta of -0.20

fit_behav_girls <- lm(X4MSCALK1 ~  X1TCHAPP, data=girlsimp)
summary(fit_behav_girls)
lm.beta(fit_behav_girls)
ggplotRegression(fit_behav_girls)
# TCHCON: std. beta of 0.21
# TCHAPP: std. beta of 0.41
# TCHEXT: std. beta of -0.17

fit_boys <- lm(X4MSCALK1 ~ X1MSCALK1 + X1TCHAPP + X4SESL_I, 
               T1D_decile=="1", 
               data=boysimp)
summary(fit_boys)
lm.beta(fit_boys)

fit_boys <- lm(X4MSCALK1 ~ X1MSCALK1 + X1TCHAPP + X4SESL_I, 
               T1D_decile=="10", 
               data=boysimp)
# summary(fit_boys)
lm.beta(fit_boys)

ggplotRegression(fit_boys)

fit_girls <- lm(X4MSCALK1 ~ X1MSCALK1 + X1TCHAPP + X4SESL_I, 
                T1D_decile=="1", 
                data=girlsimp)
# summary(fit_girls)
lm.beta(fit_girls)

ggplotRegression(fit_girls)

# quantile plots

quantile(completeimp0519$X4SESL_I, c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1))
# -2.33 -1.07 -0.80 -0.59 -0.38 -0.18  0.08  0.36  0.67  1.05  2.37

completeimp0519$SES_decile <- cut(completeimp0519$X4SESL_I, c(-2.34, -1.07, -0.80, -0.59, -0.38, 
                                                              -0.18, 0.08, 0.36, 0.67, 1.05, 2.37))
levels(completeimp0519$SES_decile) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")


###############################################################
### SIMPLE REGRESSIONS DIVIDED BY BOYS AND GIRLS ##############
###############################################################

# Does T1 teacher ratings predict achievement better for girls or boys?

fit_T1_X2_B <- lm(X2MSCALK1~T1MARSAV, data=boysimp)
ggplotRegression(fit_T1_X2_B)
lm.beta(fit_T1_X2_B)

fit_T1_X2_G <- lm(X2MSCALK1~T1MARSAV, data=girlsimp)
ggplotRegression(fit_T1_X2_G)
lm.beta(fit_T1_X2_G)
# At time 2, it's more predictive for boys

fit_T1_X4_B <- lm(X4MSCALK1~T1MARSAV, data=boysimp)
ggplotRegression(fit_T1_X4_B)
lm.beta(fit_T1_X4_B)

fit_T1_X4_G <- lm(X4MSCALK1~T1MARSAV, data=girlsimp)
ggplotRegression(fit_T1_X4_G)
lm.beta(fit_T1_X4_G)
# At time 4, it's also more predictive for boys. 


# Does T1 DA scores predict achievement better for girls or boys?

fit_X1_X2_B <- lm(X2MSCALK1~X1MSCALK1, data=boysimp)
ggplotRegression(fit_X1_X2_B)
lm.beta(fit_X1_X2_B)

fit_X1_X2_G <- lm(X2MSCALK1~X1MSCALK1, data=girlsimp)
ggplotRegression(fit_X1_X2_G)
lm.beta(fit_X1_X2_G)
# At time 2, it's more predictive for boys

fit_X1_X4_B <- lm(X4MSCALK1~X1MSCALK1, data=boysimp)
ggplotRegression(fit_X1_X4_B)
lm.beta(fit_X1_X4_B)

fit_X1_X4_G <- lm(X4MSCALK1~X1MSCALK1, data=girlsimp)
ggplotRegression(fit_X1_X4_G)
lm.beta(fit_X1_X4_G)
# At time 4, it's also more predictive for boys. 



#############################################################################
### ACHIEVEMENT PREDICTIONS SORTED BY STUFF FOR BOYS AND GIRLS ##############
#############################################################################

# Girls: T4 DA predcited by T1 DA, sorted by diff score decile
ggplot(girlsimp, aes(x=X1MSCALK1, y=X4MSCALK1, color=T1D_decile)) +
  coord_cartesian(xlim=c(6,80), ylim=c(15,96)) +
  geom_point(alpha=0.2, position=position_jitter()) + stat_smooth(method="lm", se=F) +
  scale_color_manual(name="Diff Score Decile", values=c("1"="#490092","2"="#006DDB","3"="#B66DFF","4"="#6DB6FF",
                                             "5"="#B6DBFF", "6"="#920000","7"="#924900","8"="#E6AF50",
                                             "9"="#24FF24", "10"="#FFFF6D"), 
                     labels=c("First Decile", "Second Decile","Third Decile","Fourth Decile",
                              "Fifth Decile","Sixth Decile","Seventh Decile","Eighth Decile",
                              "Ninth Decile","Tenth Decile")) +
  labs(title="Girls: Time 4 DA score by Time 1 DA")

g4 <- lm(X4MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=girlsimp)
summary(g4)
lm.beta(g4)

# Boys: T4 DA predcited by T1 DA, sorted by T1 Diff Score decile
ggplot(boysimp, aes(x=X1MSCALK1, y=X4MSCALK1, color=T1D_decile)) +
  coord_cartesian(xlim=c(6,80), ylim=c(15,96)) +
  geom_point(alpha=0.2, position=position_jitter()) + stat_smooth(method="lm", se=F) +
  scale_color_manual(name="Diff Score Decile", values=c("1"="#490092","2"="#006DDB","3"="#B66DFF","4"="#6DB6FF",
                                                        "5"="#B6DBFF", "6"="#920000","7"="#924900","8"="#E6AF50",
                                                        "9"="#24FF24", "10"="#FFFF6D"), 
                     labels=c("First Decile", "Second Decile","Third Decile","Fourth Decile",
                              "Fifth Decile","Sixth Decile","Seventh Decile","Eighth Decile",
                              "Ninth Decile","Tenth Decile")) +
  labs(title="Boys: Time 4 DA score by Time 1 DA")

b4 <- lm(X4MSCALK1 ~ T1MARSAV + T1MDIFF, data=boysimp)
summary(b4)
lm.beta(b4)

# Girls: T2 DA by T1 DA, sorted by T1 Diff Score decile
ggplot(girlsimp, aes(x=X1MSCALK1, y=X2MSCALK1, color=T1D_decile)) +
  coord_cartesian(xlim=c(6,80), ylim=c(5,86)) +
  geom_point(alpha=0.2, position=position_jitter()) + stat_smooth(method="lm", se=F) +
  scale_color_manual(name="Diff Score Decile", values=c("1"="#490092","2"="#006DDB","3"="#B66DFF","4"="#6DB6FF",
                                                        "5"="#B6DBFF", "6"="#920000","7"="#924900","8"="#E6AF50",
                                                        "9"="#24FF24", "10"="#FFFF6D"), 
                     labels=c("First Decile", "Second Decile","Third Decile","Fourth Decile",
                              "Fifth Decile","Sixth Decile","Seventh Decile","Eighth Decile",
                              "Ninth Decile","Tenth Decile")) +
  labs(title="Girls: Time 2 DA score by Time 1 DA")

g2 <- lm(X2MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=girlsimp)
summary(g2)
lm.beta(g2)

# Boys: T2 DA by T1 DA, sorted by T1 Diff Score decile
ggplot(boysimp, aes(x=X1MSCALK1, y=X2MSCALK1, color=T1D_decile)) +
  coord_cartesian(xlim=c(6,80), ylim=c(5,86)) +
  geom_point(alpha=0.2, position=position_jitter()) + stat_smooth(method="lm", se=F) +
  scale_color_manual(name="Diff Score Decile", values=c("1"="#490092","2"="#006DDB","3"="#B66DFF","4"="#6DB6FF",
                                                        "5"="#B6DBFF", "6"="#920000","7"="#924900","8"="#E6AF50",
                                                        "9"="#24FF24", "10"="#FFFF6D"), 
                     labels=c("First Decile", "Second Decile","Third Decile","Fourth Decile",
                              "Fifth Decile","Sixth Decile","Seventh Decile","Eighth Decile",
                              "Ninth Decile","Tenth Decile")) +
  labs(title="Boys: Time 2 DA score by Time 1 DA")

b2 <- lm(X2MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=boysimp)
summary(b2)
lm.beta(b2)

### Comparing top to bottom deciles
b10_T4 <- lm(X4MSCALK1 ~ X1MSCALK1, T1D_decile=="10", data=boysimp)
summary(b10_T4)
lm.beta(b10_T4)
b1_T4 <- lm(X4MSCALK1 ~ X1MSCALK1, T1D_decile=="1", data=boysimp)
summary(b1_T4)
lm.beta(b1_T4)

g10_T4 <- lm(X4MSCALK1 ~ X1MSCALK1, T1D_decile=="10", data=girlsimp)
summary(g10_T4)
lm.beta(g10_T4)
g1_T4 <- lm(X4MSCALK1 ~ X1MSCALK1, T1D_decile=="1", data=girlsimp)
summary(g1_T4)
lm.beta(g1_T4)

b10_T2 <- lm(X2MSCALK1 ~ X1MSCALK1, T1D_decile=="10", data=boysimp)
summary(b10_T2)
lm.beta(b10_T2)
b1_T2 <- lm(X2MSCALK1 ~ X1MSCALK1, T1D_decile=="1", data=boysimp)
summary(b1_T2)
lm.beta(b1_T2)

g10_T2 <- lm(X2MSCALK1 ~ X1MSCALK1, T1D_decile=="10", data=girlsimp)
summary(g10_T2)
lm.beta(g10_T2)
g1_T2 <- lm(X2MSCALK1 ~ X1MSCALK1, T1D_decile=="1", data=girlsimp)
summary(g1_T2)
lm.beta(g1_T2)










