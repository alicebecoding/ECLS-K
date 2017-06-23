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

# Analyses in the order in my doc:
#T1 ARS
ggplot(completeimp0519, aes(x=T1MARSAV, y=X2MSCALK1)) +
  geom_point(alpha=0.2) + stat_smooth(method="lm", se=F, color="black") +
  coord_cartesian(xlim=c(1,5), ylim=c(6,96)) +
  labs(x="Time 1 Teacher Rating", y="Time 2 Direct Assessment Score", 
       title="T2 DA Score predicted by T1 Teacher Rating") 

fitT1X2 <- lm(X2MSCALK1~T1MARSAV, data=completeimp0519)
summary(fitT1X2)
lm.beta(fitT1X2)

ggplot(completeimp0519, aes(x=T1MARSAV, y=X4MSCALK1)) +
  geom_point(alpha=0.2) + stat_smooth(method="lm", se=F, color="black") +
  coord_cartesian(xlim=c(1,5), ylim=c(6,96)) +
  labs(x="Time 1 Teacher Rating", y="Time 4 Direct Assessment Score", 
       title="T4 DA Score predicted by T1 Teacher Rating") 

fitT1X4 <- lm(X4MSCALK1~T1MARSAV, data=completeimp0519)
summary(fitT1X4)
lm.beta(fitT1X4)

#T1 DA
ggplot(completeimp0519, aes(x=X1MSCALK1, y=X2MSCALK1)) +
  geom_point(alpha=0.2) + stat_smooth(method="lm", se=F, color="black") +
  coord_cartesian(xlim=c(6,80), ylim=c(6,96)) +
  labs(x="Time 1 Direct Assessment Score", y="Time 2 Direct Assessment Score", 
       title="T2 DA Score predicted by T1 DA Score") 

fitX1X2 <- lm(X2MSCALK1~X1MSCALK1, data=completeimp0519)
summary(fitX1X2)
lm.beta(fitX1X2)

ggplot(completeimp0519, aes(x=X1MSCALK1, y=X4MSCALK1)) +
  geom_point(alpha=0.2) + stat_smooth(method="lm", se=F, color="black") +
  coord_cartesian(xlim=c(6,80), ylim=c(6,96)) +
  labs(x="Time 1 Direct Assessment Score", y="Time 4 Direct Assessment Score", 
       title="T4 DA Score predicted by T1 DA Score") 

# T1 Diff, controlling for T1 DA
fitT1DX2 <- lm(X2MSCALK1~X1MSCALK1 + T1MDIFF, data=completeimp0519)
summary(fitT1DX2)
lm.beta(fitT1DX2)

fitT1DX4 <- lm(X4MSCALK1~X1MSCALK1 + T1MDIFF, data=completeimp0519)
summary(fitT1DX4)
lm.beta(fitT1DX4)

# T1 Diff, controlling for T1 DA, by gender
summary(lm(X2MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=boysimp))
lm.beta(lm(X2MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=boysimp))

summary(lm(X4MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=boysimp))
lm.beta(lm(X4MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=boysimp))

summary(lm(X2MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=girlsimp))
lm.beta(lm(X2MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=girlsimp))

summary(lm(X4MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=girlsimp))
lm.beta(lm(X4MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=girlsimp))

# T1 Diff, controlling for T1 DA and T1 ARS, by gender
summary(lm(X2MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=boysimp))
lm.beta(lm(X2MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=boysimp))

summary(lm(X4MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=boysimp))
lm.beta(lm(X4MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=boysimp))

summary(lm(X2MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=girlsimp))
lm.beta(lm(X2MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=girlsimp))

summary(lm(X4MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=girlsimp))
lm.beta(lm(X4MSCALK1~X1MSCALK1 + T1MARSAV + T1MDIFF, data=girlsimp))

# SES
summary(lm(X1MSCALK1 ~ X4SESL_I, data=boysimp))
lm.beta(lm(X1MSCALK1 ~ X4SESL_I, data=boysimp))
summary(lm(X2MSCALK1 ~ X4SESL_I, data=boysimp))
lm.beta(lm(X2MSCALK1 ~ X4SESL_I, data=boysimp))
summary(lm(X4MSCALK1 ~ X4SESL_I, data=boysimp))
lm.beta(lm(X4MSCALK1 ~ X4SESL_I, data=boysimp))

summary(lm(X1MSCALK1 ~ X4SESL_I, data=girlsimp))
lm.beta(lm(X1MSCALK1 ~ X4SESL_I, data=girlsimp))
summary(lm(X2MSCALK1 ~ X4SESL_I, data=girlsimp))
lm.beta(lm(X2MSCALK1 ~ X4SESL_I, data=girlsimp))
summary(lm(X4MSCALK1 ~ X4SESL_I, data=girlsimp))
lm.beta(lm(X4MSCALK1 ~ X4SESL_I, data=girlsimp))

# Behavior
fit_behav_boys1 <- lm(X1MSCALK1 ~ X1TCHAPP + X1TCHCON, data=boysimp) 
summary(fit_behav_boys1)
lm.beta(fit_behav_boys1)

fit_behav_boys2 <- lm(X2MSCALK1 ~ X1TCHAPP + X1TCHCON, data=boysimp) 
summary(fit_behav_boys2)
lm.beta(fit_behav_boys2)

fit_behav_boys4 <- lm(X4MSCALK1 ~ X1TCHAPP + X1TCHCON, data=boysimp) 
summary(fit_behav_boys4)
lm.beta(fit_behav_boys4)

fit_behav_girls1 <- lm(X1MSCALK1 ~ X1TCHAPP + X1TCHCON, data=girlsimp) 
summary(fit_behav_girls1)
lm.beta(fit_behav_boys1)

fit_behav_girls2 <- lm(X2MSCALK1 ~ X1TCHAPP + X1TCHCON, data=girlsimp) 
summary(fit_behav_girls2)
lm.beta(fit_behav_girls2)

fit_behav_girls4 <- lm(X4MSCALK1 ~ X1TCHAPP + X1TCHCON, data=girlsimp) 
summary(fit_behav_girls4)
lm.beta(fit_behav_girls4)

# Difference score deciles
b10_T2 <- lm(X2MSCALK1 ~ X1MSCALK1, T1D_decile=="10", data=boysimp)
summary(b10_T2)
lm.beta(b10_T2)
b1_T2 <- lm(X2MSCALK1 ~ X1MSCALK1, T1D_decile=="1", data=boysimp)
summary(b1_T2)
lm.beta(b1_T2)

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

b10_T4 <- lm(X4MSCALK1 ~ X1MSCALK1, T1D_decile=="10", data=boysimp)
summary(b10_T4)
lm.beta(b10_T4)
b1_T4 <- lm(X4MSCALK1 ~ X1MSCALK1, T1D_decile=="1", data=boysimp)
summary(b1_T4)
lm.beta(b1_T4)

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

g10_T2 <- lm(X2MSCALK1 ~ X1MSCALK1, T1D_decile=="10", data=girlsimp)
summary(g10_T2)
lm.beta(g10_T2)
g1_T2 <- lm(X2MSCALK1 ~ X1MSCALK1, T1D_decile=="1", data=girlsimp)
summary(g1_T2)
lm.beta(g1_T2)

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

g10_T4 <- lm(X4MSCALK1 ~ X1MSCALK1, T1D_decile=="10", data=girlsimp)
summary(g10_T4)
lm.beta(g10_T4)
g1_T4 <- lm(X4MSCALK1 ~ X1MSCALK1, T1D_decile=="1", data=girlsimp)
summary(g1_T4)
lm.beta(g1_T4)

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

# Difference score with T1Diff by gender
summary(lm(X2MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=boysimp))
lm.beta(lm(X2MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=boysimp))

summary(lm(X4MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=boysimp))
lm.beta(lm(X4MSCALK1 ~ X1MSCALK1 + T1MDIFF, data=boysimp))
