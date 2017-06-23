# Behavior quartile plots 


# something completely different: behavior

summary(completeimp0519$X1TCHCON)
summary(completeimp0519$X2TCHCON)
summary(completeimp0519$X4TCHCON)

hist(completeimp0519$X1TCHEXT)
hist(completeimp0519$X2TCHEXT)
hist(completeimp0519$X4TCHEXT)

hist(completeimp0519$X1TCHAPP)
hist(completeimp0519$X2TCHAPP)
hist(completeimp0519$X4TCHAPP)

# prep: quartile
completeimp0519$X1CON_quartile <- cut(completeimp0519$X1TCHCON, c(0, 2.67, 3.07, 3.5, 5))
levels(completeimp0519$X1CON_quartile) <- c("1", "2", "3", "4")
completeimp0519$X1CON_quartile <- as.ordered(completeimp0519$X1CON_quartile)
is.ordered(completeimp0519$X1CON_quartile)
utils::View(completeimp0519)
summary(completeimp0519$X1CON_quartile)


# the plot
ggplot(completeimp0519, aes(x=T1MARSAV, y=X4MSCALK1, color=X1CON_quartile)) +
  coord_cartesian(xlim=c(1, 5), ylim=c(6,96)) +
  geom_point(alpha=0.2, position=position_jitter()) + stat_smooth(method="lm", se=F) +
  
  scale_color_manual(name="Quartile", values=c("1"="#B66DFF","2"="#B6DBFF","3"="#24FF24","4"="#FFFF6D"), 
                     labels=c("First Quartile", "Second Quartile","Third Quartile","Fourth Quartile")) 

# prep: sd
quantile(completeimp0519$X1TCHCON, c(0.001, 0.022, 0.158, 0.5, 0.842, 0.978, 0.999, 1))
completeimp0519$X1CON_sd <- cut(completeimp0519$X1TCHCON, c(0, 1.75, 2.33, 3.25, 3.75, 4))
levels(completeimp0519$X1CON_sd) <- c("Below -2 SDs", "Between -2 and -1 SDs", "Between -1 and 0 SDs", 
                                      "Between 0 and 1 SDs", "Above 1 SD")
completeimp0519$X1CON_sd <- as.ordered(completeimp0519$X1CON_sd)
is.ordered(completeimp0519$X1CON_sd)
summary(completeimp0519$X1CON_sd)

# the plot
ggplot(completeimp0519, aes(x=T1MARSAV, y=X4MSCALK1, color=X1CON_sd)) +
  coord_cartesian(xlim=c(1, 5), ylim=c(6,96)) +
  geom_point(alpha=0.5, position=position_jitter()) + stat_smooth(method="lm", se=F) +
  
  scale_color_manual(name="SDs", values=c("Below -2 SDs"="#D55E00", "Between -2 and -1 SDs"="#CC79A7", 
                                          "Between -1 and 0 SDs"="#F0E442", "Between 0 and 1 SDs"="#56B4E9", 
                                          "Above 1 SD"="#0072B2"), 
                     labels=c("Below -2 SDs", "-2 to -1 SDs", "-1 to 0 SDs", 
                              "0 to 1 SDs", "Above 1 SD")) +
  labs(x="Teacher Rating",y="Direct Assessment Score") +
  labs(title="T4 Direct Assessment Score by Time 1 Teacher Rating, grouped by SD of Self-Control Score") 




# Color blind 8-color palette options: 
# "#CC79A7", "#D55E00", "#E69F00", "#F0E442", "#009E73", "#56B4E9", "#0072B2", "#999999"


