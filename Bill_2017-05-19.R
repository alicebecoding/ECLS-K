# 

library(psych)
library(ggplot2)
library(graphics)
library(lattice)
library(Hmisc)
library(dplyr)
library(tidyr)
require(mice)

# asterisk is interaction
# + is additive 



#### Time 1 DA vs. Time 4 DA

t1da_t4da <- ggplot(completeimp0519, aes(x = X1MSCALK1, y = X4MSCALK1, color = as.factor(X_CHSEX_R))) +
  geom_jitter(alpha = .2) + 
  geom_smooth(se = F, method = "lm") + 
  #facet_wrap(facets = ~X_CHSEX_R) + 
  geom_abline(slope = 1, intercept = 0)
plot(p)  

lm_t1da_t4da <- lm(formula = X4MSCALK1 ~ X1MSCALK1, data = completeimp0519)
summary(lm.1)
anova(lm.1)

# outlier:
completeimp0519[10712,"X1MSCALK1"]



##### Teacher Rating at Time 1 vs DA at time 4

p <- ggplot(completeimp0519, aes(x = T1MARSAV, y = X4MSCALK1, color = as.factor(X_CHSEX_R))) +
 # geom_jitter(alpha = .2) + 
  geom_point(alpha = .2) + 
  geom_smooth(se = F, method = "lm") + 
  facet_wrap(facets = ~X_CHSEX_R) + 
  geom_abline(slope = 1, intercept = 0)
plot(p)  



#### DA at time 4, by TA time 1 interaction w/ sex
lm.2 <- lm(formula = X4MSCALK1 ~ T1MARSAV * X_CHSEX_R, data = completeimp0519)
summary(lm.2)
anova(lm.2)
#plot(lm.1)



#### DA at Time 1 by DA at t4, colored by TA at t1, abline and fit line

p <- ggplot(completeimp0519, aes(x = X1MSCALK1, y = X4MSCALK1, color = T1MARSAV)) +
  # geom_jitter(alpha = .2) + 
  geom_jitter(alpha = .2) + 
  geom_smooth(se = F, method = "loess",color="black") + 
  scale_colour_gradientn(colours=rainbow(5)) +
  #facet_wrap(facets = ~X_CHSEX_R) + 
  geom_abline(slope = 1, intercept = 0)
plot(p)  


#### DA at Time 1 by DA at t4, colored by DIFF at t1, abline and fit line

p <- ggplot(completeimp0519, aes(x = X1MSCALK1, y = X4MSCALK1, color = T1MDIFF)) +
  # geom_jitter(alpha = .2) + 
  geom_jitter(alpha = .2) + 
  geom_smooth(se = F, method = "loess",color="black") + 
  scale_color_gradientn(colors=rainbow(6))
  #facet_wrap(facets = ~X_CHSEX_R)
plot(p)  

?scale_color_gradientn

#### some basic anovas

lm.3 <- lm(formula = X4MSCALK1 ~ T1MARSAV + X1MSCALK1, data = completeimp0519)
summary(lm.3)
anova(lm.3)

lm.4 <- lm(formula = X4MSCALK1 ~ T1MDIFF * X2MSCALK1, data = completeimp0519)
summary(lm.4)
anova(lm.4)



p <- ggplot(completeimp0519, aes(x = T1MDIFF, y = X4MSCALK1, color = X_CHSEX_R)) +
  # geom_jitter(alpha = .2) + 
  geom_jitter(alpha = .2) + 
  geom_smooth(se = F, method = "lm") 
facet_wrap(facets = ~X_CHSEX_R) + 
  #  geom_abline(slope = 1, intercept = 0)
  plot(p)  




p <- ggplot(completeimp0519, aes(x = X1MSCALK1, y = X4MSCALK1, color = X_CHSEX_R)) +
  # geom_jitter(alpha = .2) + 
  geom_jitter(alpha = .2) + 
  geom_smooth(se = F, method = "lm") 
#facet_wrap(facets = ~X_CHSEX_R) + 
#  geom_abline(slope = 1, intercept = 0)
plot(p)  

p <- ggplot(completeimp0519, aes(x = T1MARSAV, y = X4MSCALK1, color = X_CHSEX_R)) +
  # geom_jitter(alpha = .2) + 
  geom_jitter(alpha = .2) + 
  geom_smooth(se = F, method = "lm") 
#facet_wrap(facets = ~X_CHSEX_R) + 
#  geom_abline(slope = 1, intercept = 0)
plot(p)  


p <- ggplot(completeimp0519, aes(x = X1MSCALK1, color = X_CHSEX_R)) +
  geom_histogram()
plot(p)  

p <- ggplot(completeimp0519, aes(x = T1MARSAV, color = X_CHSEX_R)) +
  geom_histogram(bins = 100)
plot(p)  


#### TA t1 vs DA t1, option for faceted by gender
t1m <- scale(completeimp0519$T1MARSAV, center=T,scale=T)
x1m <- scale(completeimp0519$X1MSCALK1, center=T,scale=T)
d <- cbind(completeimp0519, t1m, x1m)
p <- ggplot(d, aes(x = t1m, y = x1m, color = X4MSCALK1)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  # geom_jitter(alpha = .2) + 
  geom_jitter(alpha = .2) + 
  geom_smooth(se = F, method = "lm") +
  #facet_wrap(facets = ~X_CHSEX_R) + 
  geom_abline(slope = 1, intercept = 0)
plot(p)  


#### Teacher ratings t1 vs direct t1, colored by t4 direct
p <- ggplot(completeimp0519, aes(x = T1MARSAV, y = X1MSCALK1, color = X4MSCALK1)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  # geom_jitter(alpha = .2) + 
  geom_jitter(alpha = .2) + 
  geom_smooth(se = F, method = "lm") +
  #facet_wrap(facets = ~X_CHSEX_R) + 
  geom_abline(slope = 1, intercept = 0)
plot(p)  


#### The above faceted by gender
p <- ggplot(d, aes(x = t1m, y = x1m, color = X4MSCALK1)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  # geom_jitter(alpha = .2) + 
  geom_jitter(alpha = .2) + 
  geom_smooth(se = F, method = "lm") +
  facet_wrap(facets = ~X_CHSEX_R) + 
  geom_abline(slope = 1, intercept = 0)
plot(p)  


####################################################################################################
##### OVERLAID HISTOGRAMS: PRACTICE ################################################################
####################################################################################################

allImp0522 <- complete(imp0517,action="long", include=F)

X1MSCAL_M <- filter(allImp0522, X_CHSEX_R==0) %>% select(X1MSCALK1) %>% unlist
X1MSCAL_F <- filter(allImp0522, X_CHSEX_R==1) %>% select(X1MSCALK1) %>% unlist

# In base R
hist(X1MSCAL_M, col=rgb(1,0,0,0.5))
hist(X1MSCAL_F, col=rgb(0,0,1,0.5), add=T)

# In ggplot2
ggplot(allImp0522,aes(x=X1MSCALK1, fill=X_CHSEX_R)) + 
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "0"),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "1"),fill = "blue", alpha = 0.2)
# this has no legend

# Another way: 
ggplot(allImp0522, aes(x=X1MSCALK1, fill=X_CHSEX_R)) +
  geom_histogram(alpha=0.2) +
  scale_fill_manual(name="Sex", values=c("blue", "darkgray"), labels=c("Male","female"))
# ...this has a legend but the data is stacked!

# A third way: 
ggplot(allImp0522, aes(x=X1MSCALK1)) + 
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "0"),aes(fill = X_CHSEX_R), alpha = 0.5) +
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "1"),aes(fill = X_CHSEX_R), alpha = 0.5) + 
  scale_fill_manual(name="Sex", values=c("steelblue1", "gray46"), labels=c("Male","female"))
# Does the job. 



####################################################################################################
##### OVERLAID HISTOGRAMS: THE DATA ################################################################
####################################################################################################



#### BY GENDER

# X1MSCALK1 by gender
ggplot(allImp0522, aes(x=X1MSCALK1)) + 
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "0"),aes(fill = X_CHSEX_R), alpha = 0.5) +
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "1"),aes(fill = X_CHSEX_R), alpha = 0.5) + 
  scale_fill_manual(name="Sex", values=c("steelblue4", "gray46"), labels=c("Male","Female")) +
  labs(x="Scale Score",y="Count") +
  labs(title="Time 1 Direct Assessment Scores by Gender") +
  labs(caption="(Figure displays pooled data from m=40 imputations.)")

# X4MSCALK1 by gender
ggplot(allImp0522, aes(x=X4MSCALK1)) + 
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "0"),aes(fill = X_CHSEX_R), alpha = 0.5) +
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "1"),aes(fill = X_CHSEX_R), alpha = 0.5) + 
  scale_fill_manual(name="Sex", values=c("steelblue4", "gray46"), labels=c("Male","female")) +
  labs(x="Scale Score",y="Count") +
  labs(title="Time 4 Direct Assessment Scores by Gender") +
  labs(caption="(Figure displays pooled data from m=40 imputations.)")

# T1MARSAV by gender
ggplot(allImp0522, aes(x=T1MARSAV)) + 
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "0"),aes(fill = X_CHSEX_R), alpha = 0.5, 
                 binwidth = 0.25) +
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "1"),aes(fill = X_CHSEX_R), alpha = 0.5, 
                 binwidth = 0.25) + 
  scale_fill_manual(name="Sex", values=c("steelblue4", "gray46"), labels=c("Male","female")) +
  labs(x="Average Score",y="Rescaled Count") +
  labs(title="Time 1 Teacher Assessment Scores by Gender") +
  labs(caption="(Figure displays pooled data from m=40 imputations.)")


# T4MARSAV by gender
ggplot(allImp0522, aes(x=T4MARSAV)) + 
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "0"),aes(fill = X_CHSEX_R), alpha = 0.5, 
                 binwidth = 0.25) +
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "1"),aes(fill = X_CHSEX_R), alpha = 0.5, 
                 binwidth = 0.25) +
  scale_fill_manual(name="Sex", values=c("steelblue4", "gray46"), labels=c("Male","female")) +
  labs(x="Average Score",y="Rescaled Count") +
  labs(title="Time 4 Teacher Assessment Scores by Gender") +
  labs(caption="(Figure displays pooled data from m=40 imputations.)")



# T1MDIFF by gender
ggplot(allImp0522, aes(x=T1MDIFF)) + 
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "0"),aes(fill = X_CHSEX_R), alpha = 0.5) +
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "1"),aes(fill = X_CHSEX_R), alpha = 0.5) + 
  scale_fill_manual(name="Sex", values=c("steelblue4", "gray46"), labels=c("Male","female")) +
  labs(x="Difference Score",y="Rescaled Count") +
  labs(title="Time 1 Difference Scores (Teacher - Direct) by Gender") +
  labs(caption="(Figure displays pooled data from m=40 imputations.)")


# T4MDIFF by gender
ggplot(allImp0522, aes(x=T4MDIFF)) + 
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "0"),aes(fill = X_CHSEX_R), alpha = 0.5) +
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "1"),aes(fill = X_CHSEX_R), alpha = 0.5) + 
  scale_fill_manual(name="Sex", values=c("steelblue4", "gray46"), labels=c("Male","female")) +
  labs(x="Difference Score",y="Rescaled Count") +
  labs(title="Time 4 Difference Scores (Teacher - Direct) by Gender") +
  labs(caption="(Figure displays pooled data from m=40 imputations.)")



#### BY RACE: 

# 1=White, non-hispanic
# 2=Black, non-hispanic
# 3=Hispanic, Race Specified
# 4=Hispanic, No Race Specified
# 5=Asian, non-hispanic
# 6=Native Hawaiian/Pacific Islander, non-hispanic
# 7=American Indian/Alaska Native, non-hispanic
# 8=Two or more races, non-hispanic

# X1MSCALK1 by race
ggplot(allImp0522, aes(x=X1MSCALK1,y=..ncount..)) + 
  geom_histogram(data=subset(allImp0522,X_RACETH_R == "1"),aes(fill = X_RACETH_R), alpha = 0.5) +
  geom_histogram(data=subset(allImp0522,X_RACETH_R == "2"),aes(fill = X_RACETH_R), alpha = 0.5) + 
  scale_fill_manual(name="Race", values=c("gray46", "green4"), labels=c("White","Black")) +
  labs(x="Scale Score",y="Rescaled Count") +
  labs(title="Time 1 Direct Assessment Scores by Race")

# X4MSCALK1 by race
ggplot(allImp0522, aes(x=X4MSCALK1,y=..ncount..)) + 
  geom_histogram(data=subset(allImp0522,X_RACETH_R == "1"),aes(fill = X_RACETH_R), alpha = 0.5) +
  geom_histogram(data=subset(allImp0522,X_RACETH_R == "2"),aes(fill = X_RACETH_R), alpha = 0.5) + 
  scale_fill_manual(name="Race", values=c("gray46", "green4"), labels=c("White","Black")) +
  labs(x="Scale Score",y="Rescaled Count") +
  labs(title="Time 4 Direct Assessment Scores by Race")

# T1MARSAV by race
ggplot(allImp0522, aes(x=T1MARSAV,y=..ncount..)) + 
  geom_histogram(data=subset(allImp0522,X_RACETH_R == "1"),aes(fill = X_RACETH_R), alpha = 0.5, 
                 binwidth = 0.25) +
  geom_histogram(data=subset(allImp0522,X_RACETH_R == "2"),aes(fill = X_RACETH_R),  alpha = 0.5, 
                 binwidth = 0.25) +
  scale_fill_manual(name="Race", values=c("gray46", "green4"), labels=c("White","Black")) +
  labs(x="Scale Score",y="Rescaled Count") +
  labs(title="Time 1 Teacher Assessment Scores by Race")

# T4MARSAV by race
ggplot(allImp0522, aes(x=T4MARSAV,y=..ncount..)) + 
  geom_histogram(data=subset(allImp0522,X_RACETH_R == "1"),aes(fill = X_RACETH_R), alpha = 0.5, 
                 binwidth = 0.25) +
  geom_histogram(data=subset(allImp0522,X_RACETH_R == "2"),aes(fill = X_RACETH_R), alpha = 0.5, 
                 binwidth = 0.25) +
  scale_fill_manual(name="Race", values=c("gray46", "green4"), labels=c("White","Black")) +
  labs(x="Scale Score",y="Rescaled Count") +
  labs(title="Time 4 Teacher Assessment Scores by Race")

# T1MDIFF by race
ggplot(allImp0522, aes(x=T1MDIFF,y=..ncount..)) + 
  geom_histogram(data=subset(allImp0522,X_RACETH_R == "1"),aes(fill = X_RACETH_R), alpha = 0.5) +
  geom_histogram(data=subset(allImp0522,X_RACETH_R == "2"),aes(fill = X_RACETH_R), alpha = 0.5) + 
  scale_fill_manual(name="Race", values=c("gray46", "green4"), labels=c("White","Black")) +
  labs(x="Scale Score",y="Rescaled Count") +
  labs(title="Time 1 Difference Scores (Teacher - Direct) by Race")

# T4MDIFF by race
ggplot(allImp0522, aes(x=T4MDIFF,y=..ncount..)) + 
  geom_histogram(data=subset(allImp0522,X_RACETH_R == "1"),aes(fill = X_RACETH_R), alpha = 0.5) +
  geom_histogram(data=subset(allImp0522,X_RACETH_R == "2"),aes(fill = X_RACETH_R), alpha = 0.5) + 
  scale_fill_manual(name="Race", values=c("gray46", "green4"), labels=c("White","Black")) +
  labs(x="Scale Score",y="Rescaled Count") +
  labs(title="Time 4 Difference Scores (Teacher - Direct) by Race")



#### BEHAVIOR BY GENDER 

# X1TCHAPP by gender
ggplot(allImp0522, aes(x=X1TCHAPP)) + 
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "0"),aes(fill = X_CHSEX_R), alpha = 0.5, 
                 binwidth=0.25) +
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "1"),aes(fill = X_CHSEX_R),  alpha = 0.5, 
                 binwidth=0.25) +
  scale_fill_manual(name="Sex", values=c("steelblue4", "gray46"), labels=c("Male","Female")) +
  labs(x="Scale Score",y="Count") +
  labs(title="Time 1 Approaches to Learning Scores by Gender")

# X4TCHAPP by gender
ggplot(allImp0522, aes(x=X4TCHAPP)) + 
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "0"),aes(fill = X_CHSEX_R), alpha = 0.5, 
                 binwidth=0.25) +
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "1"),aes(fill = X_CHSEX_R),  alpha = 0.5, 
                 binwidth=0.25) +
  scale_fill_manual(name="Sex", values=c("steelblue4", "gray46"), labels=c("Male","Female")) +
  labs(x="Scale Score",y="Count") +
  labs(title="Time 4 Approaches to Learning Scores by Gender")

# X1TCHCON by gender
ggplot(allImp0522, aes(x=X1TCHCON)) + 
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "0"),aes(fill = X_CHSEX_R), alpha = 0.5, 
                 binwidth=0.25) +
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "1"),aes(fill = X_CHSEX_R),  alpha = 0.5, 
                 binwidth=0.25) +
  scale_fill_manual(name="Sex", values=c("steelblue4", "gray46"), labels=c("Male","Female")) +
  labs(x="Scale Score",y="Count") +
  labs(title="Time 1 Self Control Scores by Gender")

# X4TCHCON by gender
ggplot(allImp0522, aes(x=X4TCHCON)) + 
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "0"),aes(fill = X_CHSEX_R), alpha = 0.5, 
                 binwidth=0.25) +
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "1"),aes(fill = X_CHSEX_R),  alpha = 0.5, 
                 binwidth=0.25) +
  scale_fill_manual(name="Sex", values=c("steelblue4", "gray46"), labels=c("Male","Female")) +
  labs(x="Scale Score",y="Count") +
  labs(title="Time 4 Self Control Scores by Gender")

# X1TCHEXT by gender
ggplot(allImp0522, aes(x=X1TCHEXT)) + 
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "0"),aes(fill = X_CHSEX_R), alpha = 0.5, 
                 binwidth=0.25) +
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "1"),aes(fill = X_CHSEX_R),  alpha = 0.5, 
                 binwidth=0.25) +
  scale_fill_manual(name="Sex", values=c("steelblue4", "gray46"), labels=c("Male","Female")) +
  labs(x="Scale Score",y="Count") +
  labs(title="Time 1 Externalizing Behavior Scores by Gender")

# X4TCHEXT by gender
ggplot(allImp0522, aes(x=X4TCHEXT)) + 
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "0"),aes(fill = X_CHSEX_R), alpha = 0.5, 
                 binwidth=0.25) +
  geom_histogram(data=subset(allImp0522,X_CHSEX_R == "1"),aes(fill = X_CHSEX_R),  alpha = 0.5, 
                 binwidth=0.25) +
  scale_fill_manual(name="Sex", values=c("steelblue4", "gray46"), labels=c("Male","Female")) +
  labs(x="Scale Score",y="Count") +
  labs(title="Time 4 Externalizing Behavior Scores by Gender")



# Linear model: X4 DA by T1 Diff, subsetted by X1 DA
ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1)) +
  geom_point(data=subset(completeimp0519,X1MSCALK1<="29.48"), alpha=0.2, color="blue") +
  geom_point(data=subset(completeimp0519,X1MSCALK1 > "29.48"), alpha=0.2) +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1<="29.48"), color="blue") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"29.48"), color="black") +
  labs(x="Difference Score (Teacher - Direct)",y="Time 4 DA Scale Score") +
  labs(title="Time 4 DA score, predicted by Time 1 Difference Score, subsetted by Time 1 DA score")



#### X4 ~ T1 grouped by X1 quantile



# Tryna add a legend
ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1)) +
  geom_point(data=subset(completeimp0519,X1MSCALK1<="22.29"), alpha=0.2, aes(color=X1MSCALK1_f)) +
  geom_point(data=subset(completeimp0519,X1MSCALK1<="29.48"&X1MSCALK1>"22.29"), 
             alpha=0.2, aes(color=X1MSCALK1_f)) +
  geom_point(data=subset(completeimp0519,X1MSCALK1<="37.13"&X1MSCALK1>"29.48"), 
             alpha=0.2, aes(color=X1MSCALK1_f)) +
  geom_point(data=subset(completeimp0519,X1MSCALK1 > "37.13"), alpha=0.2, aes(color=X1MSCALK1_f)) +
  scale_color_manual(name="Quantile", values=c("green","blue","purple","black"), 
                     labels=c("1st Quartile","2nd Quartile","3rd Quartile","4th Quartile")) +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1<="22.29"), 
              color="green") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1<="29.48"&X1MSCALK1>"22.29"), 
              color="blue") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1<="37.13"&X1MSCALK1>"29.48"), 
              color="purple") +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519,X1MSCALK1>"37.13"), color="black") +
  labs(x="Difference Score (Teacher - Direct)",y="Time 4 DA Scale Score") +
  labs(title="Time 1 Difference Score vs Time 4 DA score, grouped by Time 1 DA score")

completeimp0519$X1MSCALK1_f <- cut(completeimp0519$X1MSCALK1, c(0, 22.29, 29.48, 37.13, 100))
quantile(completeimp0519$X1MSCALK1)
summary(completeimp0519$X1MSCALK1)
levels(completeimp0519$X1MSCALK1_f) <- c("First Quartile", "Second Quartile","Third Quartile",
                                         "Fourth Quartile")

first <- filter(completeimp0519, X1MSCALK1_f=="First Quartile") %>% select(c(X4MSCALK1, T1MDIFF)) 
second <- filter(completeimp0519, X1MSCALK1_f=="Second Quartile") %>% select(c(X4MSCALK1, T1MDIFF))  
third <- filter(completeimp0519, X1MSCALK1_f=="Third Quartile") %>% select(c(X4MSCALK1, T1MDIFF))  
fourth <- filter(completeimp0519, X1MSCALK1_f=="Fourth Quartile") %>% select(c(X4MSCALK1, T1MDIFF)) 

corr.test(first)
cor(second)
cor(third)
cor(fourth)

fitfirst <- lm(first$X4MSCALK1 ~ first$T1MDIFF)
summary(fitfirst)

fitsecond <- lm(second$X4MSCALK1 ~ second$T1MDIFF)
summary(fitsecond)

fitthird <- lm(third$X4MSCALK1 ~ third$T1MDIFF)
summary(fitthird)

fitfourth <- lm(fourth$X4MSCALK1 ~ fourth$T1MDIFF)
summary(fitfourth)



#### Do by ...decile? 10 percent ranges


library(Rcmdr)
first <- filter(completeimp0519, X1MSCALK1_f=="First Quartile") %>% select(c(X4MSCALK1, T1MDIFF)) 
second <- filter(completeimp0519, X1MSCALK1_f=="Second Quartile") %>% select(c(X4MSCALK1, T1MDIFF))  
third <- filter(completeimp0519, X1MSCALK1_f=="Third Quartile") %>% select(c(X4MSCALK1, T1MDIFF))  
fourth <- filter(completeimp0519, X1MSCALK1_f=="Fourth Quartile") %>% select(c(X4MSCALK1, T1MDIFF)) 

corr.test(first)
cor(second)
cor(third)
cor(fourth)

fitfirst <- lm(first$X4MSCALK1 ~ first$T1MDIFF)
summary(fitfirst)

fitsecond <- lm(second$X4MSCALK1 ~ second$T1MDIFF)
summary(fitsecond)

fitthird <- lm(third$X4MSCALK1 ~ third$T1MDIFF)
summary(fitthird)

fitfourth <- lm(fourth$X4MSCALK1 ~ fourth$T1MDIFF)
summary(fitfourth)

