# Individual plots: T1MDIFF by X4MSCAL, by decile of X1MSCAL

# Trying just individual pieces

ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1))  + 
  coord_cartesian(xlim=c(-3.5, 5.5), ylim=c(12,100)) +
  geom_point(data=subset(completeimp0519, X1M_decile=="1"), aes(color="490092"), alpha=0.2) +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519, X1M_decile=="1"), aes(color="490092")) + 
  
  scale_color_manual(name="Decile", values=c("#490092"), 
                     labels=c("First Decile"))

ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1))  + 
  coord_cartesian(xlim=c(-3.5, 5.5), ylim=c(12,100)) +
  geom_point(data=subset(completeimp0519, X1M_decile=="2"), aes(color="006DDB"), alpha=0.2) +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519, X1M_decile=="2"), aes(color="006DDB")) + 
  
  scale_color_manual(name="Decile", values=c("#006DDB"), 
                     labels=c("Second Decile"))

ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1))  + 
  coord_cartesian(xlim=c(-3.5, 5.5), ylim=c(12,100)) +
  geom_point(data=subset(completeimp0519, X1M_decile=="3"), aes(color="B66DFF"), alpha=0.2) +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519, X1M_decile=="3"), aes(color="B66DFF")) + 
  
  scale_color_manual(name="Decile", values=c("#B66DFF"), 
                     labels=c("Third Decile"))

ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1))  + 
  coord_cartesian(xlim=c(-3.5, 5.5), ylim=c(12,100)) +
  geom_point(data=subset(completeimp0519, X1M_decile=="4"), aes(color="6DB6FF"), alpha=0.2) +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519, X1M_decile=="4"), aes(color="6DB6FF")) + 
  
  scale_color_manual(name="Decile", values=c("#6DB6FF"), 
                     labels=c("Fourth Decile"))

ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1))  + 
  coord_cartesian(xlim=c(-3.5, 5.5), ylim=c(12,100)) +
  geom_point(data=subset(completeimp0519, X1M_decile=="5"), aes(color="B6DBFF"), alpha=0.2) +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519, X1M_decile=="5"), aes(color="B6DBFF")) + 
  
  scale_color_manual(name="Decile", values=c("#B6DBFF"), 
                     labels=c("Fifth Decile"))

ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1))  + 
  coord_cartesian(xlim=c(-3.5, 5.5), ylim=c(12,100)) +
  geom_point(data=subset(completeimp0519, X1M_decile=="6"), aes(color="920000"), alpha=0.2) +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519, X1M_decile=="6"), aes(color="920000")) + 
  
  scale_color_manual(name="Decile", values=c("#920000"), 
                     labels=c("Sixth Decile"))

ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1))  + 
  coord_cartesian(xlim=c(-3.5, 5.5), ylim=c(12,100)) +
  geom_point(data=subset(completeimp0519, X1M_decile=="7"), aes(color="924900"), alpha=0.2) +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519, X1M_decile=="7"), aes(color="924900")) + 
  
  scale_color_manual(name="Decile", values=c("#924900"), 
                     labels=c("Seventh Decile"))

ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1))  + 
  coord_cartesian(xlim=c(-3.5, 5.5), ylim=c(12,100)) +
  geom_point(data=subset(completeimp0519, X1M_decile=="8"), aes(color="DBD100"), alpha=0.2) +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519, X1M_decile=="8"), aes(color="DBD100")) + 
  
  scale_color_manual(name="Decile", values=c("#DBD100"), 
                     labels=c("Eighth Decile"))

ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1))  + 
  coord_cartesian(xlim=c(-3.5, 5.5), ylim=c(12,100)) +
  geom_point(data=subset(completeimp0519, X1M_decile=="9"), aes(color="24FF24"), alpha=0.2) +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519, X1M_decile=="9"), aes(color="24FF24")) + 
  
  scale_color_manual(name="Decile", values=c("#24FF24"), 
                     labels=c("Ninth Decile"))

ggplot(completeimp0519, aes(x=T1MDIFF, y=X4MSCALK1))  + 
  coord_cartesian(xlim=c(-3.5, 5.5), ylim=c(12,100)) +
  geom_point(data=subset(completeimp0519, X1M_decile=="10"), aes(color="FFFF6D"), alpha=0.2) +
  geom_smooth(method="lm", se=F, data=subset(completeimp0519, X1M_decile=="10"), aes(color="FFFF6D")) + 
  
  scale_color_manual(name="Decile", values=c("#FFFF6D"), 
                     labels=c("Tenth Decile"))
