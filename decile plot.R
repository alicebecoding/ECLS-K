# plot by decile

# 0 is male, 1 is female
# 1=White, non-hispanic
# 2=Black, non-hispanic
# 3=Hispanic, Race Specified
# 4=Hispanic, No Race Specified
# 5=Asian, non-hispanic
# 6=Native Hawaiian/Pacific Islander, non-hispanic
# 7=American Indian/Alaska Native, non-hispanic
# 8=Two or more races, non-hispanic

library(psych)
library(ggplot2)
library(graphics)
library(lattice)
library(Hmisc)
library(dplyr)
library(tidyr)
require(mice)

summary(completeimp0519$X1MSCALK1)
quantile(completeimp0519$X1MSCALK1, c(.1, .2, .3, .4, .5, .6, .7, .8, .9))

completeimp0519$X1M_decile <- cut(completeimp0519$X1MSCALK1, c(0, 16.58210, 20.84860, 23.66760, 
                                                                  26.48316, 29.48160, 32.19022, 
                                                                  35.10866, 39.58508, 45.08063, 100))
levels(completeimp0519$X1M_decile) <- c("First Decile", "Second Decile","Third Decile",
                                         "Fourth Decile","Fifth Decile","Sixth Decile",
                                         "Seventh Decile","Eighth Decile","Ninth Decile","Tenth Decile")
utils::View(completeimp0519)
summary(completeimp0519$X1M_decile)

# Plot divided into deciles

ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1)) +
  geom_point(data=subset(completeimp0519,X1MSCALK1<="16.58210"), alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1MSCALK1>"16.58210"&X1MSCALK1<="20.84860"), 
             alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1MSCALK1>"20.84860"&X1MSCALK1<="23.66760"), 
             alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1MSCALK1>"23.66760"&X1MSCALK1<="26.48316"), 
             alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1MSCALK1>"26.48316"&X1MSCALK1<="29.48160"), 
             alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1MSCALK1>"29.48160"&X1MSCALK1<="32.19022"), 
             alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1MSCALK1>"32.19022"&X1MSCALK1<="35.10866"), 
             alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1MSCALK1>"35.10866"&X1MSCALK1<="39.58508"), 
             alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1MSCALK1>"39.58508"&X1MSCALK1<="45.08063"), 
             alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1MSCALK1>"45.08063"), 
             alpha=0.2, aes(color=X1M_decile)) +
  
#  scale_color_manual(name="Quantile", values=c("#490092","#006DDB","#B66DFF","#6DB6FF","#B6DBFF",
#                                               "#920000","#924900","#DBD100","#24FF24","#FFFF6D"), 
#                     labels=c("First Decile", "Second Decile","Third Decile","Fourth Decile",
#                              "Fifth Decile","Sixth Decile","Seventh Decile","Eighth Decile",
#                              "Ninth Decile","Tenth Decile")) +
  scale_color_manual(name="Quantile", values=c("#490092","#006DDB","#B66DFF","#6DB6FF","#B6DBFF",
                                              "#920000","#924900","#DBD100","#24FF24","#FFFF6D"), 
                     labels=c("First Decile", "Second Decile","Third Decile","Fourth Decile",
                              "Fifth Decile","Sixth Decile","Seventh Decile","Eighth Decile",
                              "Ninth Decile","Tenth Decile")) +
  
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1<="16.58210"), color="#490092") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"16.58210"&X1MSCALK1<="20.84860"), color="#006DDB") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"20.84860"&X1MSCALK1<="23.66760"), color="#B66DFF") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"23.66760"&X1MSCALK1<="26.48316"), color="#6DB6FF") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"26.48316"&X1MSCALK1<="29.48160"), color="#B6DBFF") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"29.48160"&X1MSCALK1<="32.19022"), color="#920000") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"32.19022"&X1MSCALK1<="35.10866"), color="#924900") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"35.10866"&X1MSCALK1<="39.58508"), color="#DBD100") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"39.58508"&X1MSCALK1<="45.08063"), color="#24FF24") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"45.08063"), color="#FFFF6D") +

    labs(x="Difference Score (Teacher - Direct)",y="Time 4 DA Scale Score") +
  labs(title="Time 1 Difference Score vs Time 4 DA score, grouped by Time 1 DA score")








# Different colors, and lower 90th percentile by top 10th percentile

completeimp0519$X1M_90_10 <- cut(completeimp0519$X1MSCALK1, c(0, 45.08063, 100))
levels(completeimp0519$X1M_90_10) <- c("Lower 90th","Top 10th")

ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1)) +
#  geom_point(data=subset(completeimp0519,X1MSCALK1<="16.58210"), alpha=0.2, aes(color=X1M_decile)) +
#  geom_point(data=subset(completeimp0519,X1MSCALK1>"16.58210"&X1MSCALK1<="20.84860"), 
#             alpha=0.2, aes(color=X1M_decile)) +
#  geom_point(data=subset(completeimp0519,X1MSCALK1>"20.84860"&X1MSCALK1<="23.66760"), 
#             alpha=0.2, aes(color=X1M_decile)) +
#  geom_point(data=subset(completeimp0519,X1MSCALK1>"23.66760"&X1MSCALK1<="26.48316"), 
#             alpha=0.2, aes(color=X1M_decile)) +
#  geom_point(data=subset(completeimp0519,X1MSCALK1>"26.48316"&X1MSCALK1<="29.48160"), 
#             alpha=0.2, aes(color=X1M_decile)) +
#  geom_point(data=subset(completeimp0519,X1MSCALK1>"29.48160"&X1MSCALK1<="32.19022"), 
#             alpha=0.2, aes(color=X1M_decile)) +
#  geom_point(data=subset(completeimp0519,X1MSCALK1>"32.19022"&X1MSCALK1<="35.10866"), 
#             alpha=0.2, aes(color=X1M_decile)) +
#  geom_point(data=subset(completeimp0519,X1MSCALK1>"35.10866"&X1MSCALK1<="39.58508"), 
#             alpha=0.2, aes(color=X1M_decile)) +
#  geom_point(data=subset(completeimp0519,X1MSCALK1>"39.58508"&X1MSCALK1<="45.08063"), 
#             alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1MSCALK1<="45.08063"), 
             alpha=0.2, aes(color=X1M_90_10)) +
geom_point(data=subset(completeimp0519,X1MSCALK1>"45.08063"), 
             alpha=0.2, aes(color=X1M_90_10)) +
  
  # "#490092","#006DDB","#B66DFF","#6DB6FF", "#B6DBFF",
  # "#920000","#924900","#DBD100","#24FF24",
  
  scale_color_manual(name="Quantile", values=c("#6DB6FF","#FFFF6D"), 
                     
  # "First Decile", "Second Decile","Third Decile","Fourth Decile",
  # "Fifth Decile","Sixth Decile","Seventh Decile","Eighth Decile",
  # "Ninth Decile",
                     
                     labels=c("One thru Nine","Tenth Decile")) +
  
#  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1<="16.58210"), color="#490092") +
#  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"16.58210"&X1MSCALK1<="20.84860"), color="#006DDB") +
#  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"20.84860"&X1MSCALK1<="23.66760"), color="#B66DFF") +
#  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"23.66760"&X1MSCALK1<="26.48316"), color="#6DB6FF") +
#  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"26.48316"&X1MSCALK1<="29.48160"), color="#B6DBFF") +
#  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"29.48160"&X1MSCALK1<="32.19022"), color="#920000") +
#  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"32.19022"&X1MSCALK1<="35.10866"), color="#924900") +
#  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"35.10866"&X1MSCALK1<="39.58508"), color="#DBD100") +
#  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"39.58508"&X1MSCALK1<="45.08063"), color="#24FF24") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1<="45.08063"), color="#6DB6FF") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"45.08063"), color="#FFFF6D") +
  
  labs(x="Difference Score (Teacher - Direct)",y="Time 4 DA Scale Score") +
  labs(title="Time 1 Difference Score vs Time 4 DA score, grouped by Time 1 DA score")



# WAY SIMPLER OPTION?!?!?!

ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1)) +
  geom_point(data=subset(completeimp0519,X1M_decile=="First Decile"), alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1M_decile=="Second Decile"), alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1M_decile=="Third Decile"), alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1M_decile=="Fourth Decile"), alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1M_decile=="Fifth Decile"), alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1M_decile=="Sixth Decile"), alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1M_decile=="Seventh Decile"), alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1M_decile=="Eighth Decile"), alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1M_decile=="Ninth Decile"), alpha=0.2, aes(color=X1M_decile)) +
  geom_point(data=subset(completeimp0519,X1M_decile=="Tenth Decile"), alpha=0.2, aes(color=X1M_decile)) +
  
  scale_color_manual(name="Decile", values=c("#490092","#006DDB","#B66DFF","#6DB6FF","#B6DBFF",
                                               "#920000","#924900","#DBD100","#24FF24","#FFFF6D"), 
                    labels=c("First Decile","Second Decile","Third Decile","Fourth Decile",
                             "Fifth Decile","Sixth Decile","Seventh Decile","Eighth Decile",
                             "Ninth Decile","Tenth Decile")) +

  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1M_decile=="First Decile"), color="#490092") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1M_decile=="Second Decile"), color="#006DDB") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1M_decile=="Third Decile"), color="#B66DFF") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1M_decile=="Fourth Decile"), color="#6DB6FF") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1M_decile=="Fifth Decile"), color="#B6DBFF") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1M_decile=="Sixth Decile"), color="#920000") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1M_decile=="Seventh Decile"), color="#924900") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1M_decile=="Eighth Decile"), color="#DBD100") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1M_decile=="Ninth Decile"), color="#24FF24") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1M_decile=="Tenth Decile"), color="#FFFF6D") +
  
  labs(x="Difference Score (Teacher - Direct)",y="Time 4 DA Scale Score") +
  labs(title="Time 1 Difference Score vs Time 4 DA score, grouped by Time 1 DA score")



