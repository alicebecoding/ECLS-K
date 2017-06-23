# Overlaid histograms

# Subset by gender
f3female <- f3[which(f3$X_CHSEX_R==2),]
f3male <- f3[which(f3$X_CHSEX_R==1),]

f3testfemale <- f3_test[which(f3$X_CHSEX_R==2),]
f3testmale <- f3_test[which(f3$X_CHSEX_R==1),]

# Time 1 math DA scale scores by gender
hist(f3female$X1MSCALK1, col=rgb(0.05,0.05,0.05,0.5), main="Fall Kindergarten Math Direct Assessment Scores by Gender",
     xlab="Scale Score", ylab="Frequency", breaks=seq(5,95,by=5))
hist(f3male$X1MSCALK1, col=rgb(0.7,0.7,0.7,0.5), add=T)
legend("topright", c("Female", "Male"), col=c(rgb(0.05,0.05,0.05,0.5),rgb(0.7,0.7,0.7,0.5)),lty=2, lwd=10)

# Time 2 math scores
hist(f3female$X2MSCALK1, col=rgb(0.05,0.05,0.05,0.5), main="Spring Kindergarten Math Direct Assessment Scores by Gender",
     xlab="Scale Score", ylab="Frequency", breaks=seq(5,95,by=5))
hist(f3male$X2MSCALK1, col=rgb(0.7,0.7,0.7,0.5), add=T)
legend("topright", c("Female", "Male"), col=c(rgb(0.05,0.05,0.05,0.5),rgb(0.7,0.7,0.7,0.5)),lty=2, lwd=10)

# Time 4 math scores
hist(f3female$X4MSCALK1, col=rgb(0.05,0.05,0.05,0.5), main="Spring First Grade Math Direct Assessment Scores by Gender",
     xlab="Scale Score", ylab="Frequency", breaks=seq(5,95,by=5))
hist(f3male$X4MSCALK1, col=rgb(0.7,0.7,0.7,0.5), add=T)
legend("topright", c("Female", "Male"), col=c(rgb(0.05,0.05,0.05,0.5),rgb(0.7,0.7,0.7,0.5)),lty=2, lwd=10)

# Do the same thing with normalized scores
# Time 1 math DA Z-scores by gender
hist(f3testfemale$X1MSCALK1, col=rgb(0.05,0.05,0.05,0.5), main="Fall Kindergarten Math Direct Assessment Scores by Gender",
     xlab="Z-Score", ylab="Frequency", breaks=seq(-4,4,by=0.5))
hist(f3testmale$X1MSCALK1, col=rgb(0.7,0.7,0.7,0.5), add=T)
legend("topright", c("Female", "Male"), col=c(rgb(0.05,0.05,0.05,0.5),rgb(0.7,0.7,0.7,0.5)),lty=2, lwd=10)

# Time 2 math scores
hist(f3testfemale$X2MSCALK1, col=rgb(0.05,0.05,0.05,0.5), main="Spring Kindergarten Math Direct Assessment Scores by Gender",
     xlab="Z-Score", ylab="Frequency", breaks=seq(-4,4,by=0.5))
hist(f3testmale$X2MSCALK1, col=rgb(0.7,0.7,0.7,0.5), add=T)
legend("topright", c("Female", "Male"), col=c(rgb(0.05,0.05,0.05,0.5),rgb(0.7,0.7,0.7,0.5)),lty=2, lwd=10)

# Time 4 math scores
hist(f3testfemale$X4MSCALK1, col=rgb(0.05,0.05,0.05,0.5), main="Spring First Grade Math Direct Assessment Scores by Gender",
     xlab="Z-Score", ylab="Frequency", breaks=seq(-4,4,by=0.5))
hist(f3testmale$X4MSCALK1, col=rgb(0.7,0.7,0.7,0.5), add=T)
legend("topright", c("Female", "Male"), col=c(rgb(0.05,0.05,0.05,0.5),rgb(0.7,0.7,0.7,0.5)),lty=2, lwd=10)



# Teacher Ratings
# Time 1 math ARS scores by gender
hist(f3female$T1MARS, col=rgb(0.05,0.05,0.05,0.5), main="Fall Kindergarten Math ARS Scores by Gender",
     xlab="Scale Score", ylab="Frequency", breaks=seq(1,5,by=0.5))
hist(f3male$T1MARS, col=rgb(0.7,0.7,0.7,0.5), add=T)
legend("topright", c("Female", "Male"), col=c(rgb(0.05,0.05,0.05,0.5),rgb(0.7,0.7,0.7,0.5)),lty=2, lwd=10)

# Time 2 math ARS scores
hist(f3female$T2MARS, col=rgb(0.05,0.05,0.05,0.5), main="Time 2 Math ARS Scores by Gender",
     xlab="Scale Score", ylab="Frequency", breaks=seq(1,5,by=0.5))
hist(f3male$T2MARS, col=rgb(0.7,0.7,0.7,0.5), add=T)
legend("topright", c("Female", "Male"), col=c(rgb(0.05,0.05,0.05,0.5),rgb(0.7,0.7,0.7,0.5)),lty=2, lwd=10)

# Time 4 math ARS scores
hist(f3female$T4MARS, col=rgb(0.05,0.05,0.05,0.5), main="Time 4 Math ARS Scores by Gender",
     xlab="Scale Score", ylab="Frequency", breaks=seq(1,5,by=0.5))
hist(f3male$T4MARS, col=rgb(0.7,0.7,0.7,0.5), add=T)
legend("topright", c("Female", "Male"), col=c(rgb(0.05,0.05,0.05,0.5),rgb(0.7,0.7,0.7,0.5)),lty=2, lwd=10)

# Do the same thing with normalized scores
# Time 1 math ARS scores by gender
hist(f3testfemale$T1MARS, col=rgb(0.05,0.05,0.05,0.5), main="Time 1 Math ARS Scores by Gender",
     xlab="Z-Score", ylab="Frequency", breaks=seq(-4,4,by=0.5))
hist(f3testmale$T1MARS, col=rgb(0.7,0.7,0.7,0.5), add=T, breaks=seq(-4,4,by=0.5))
legend("topright", c("Female", "Male"), col=c(rgb(0.05,0.05,0.05,0.5),rgb(0.7,0.7,0.7,0.5)),lty=2, lwd=10)

# Time 2 math ARS scores
hist(f3testfemale$T2MARS, col=rgb(0.05,0.05,0.05,0.5), main="Time 2 Math ARS Scores by Gender",
     xlab="Z-Score", ylab="Frequency",  breaks=seq(-4,4,by=0.5))
hist(f3testmale$T2MARS, col=rgb(0.7,0.7,0.7,0.5), add=T, breaks=seq(-4,4,by=0.5))
legend("topright", c("Female", "Male"), col=c(rgb(0.05,0.05,0.05,0.5),rgb(0.7,0.7,0.7,0.5)),lty=2, lwd=10)

# Time 4 math ARS scores
hist(f3testfemale$T4MARS, col=rgb(0.05,0.05,0.05,0.5), main="Time 4 Math ARS Scores by Gender",
     xlab="Z-Score", ylab="Frequency", breaks=seq(-4,4,by=0.5))
hist(f3testmale$T4MARS, col=rgb(0.7,0.7,0.7,0.5), add=T, breaks=seq(-4,4,by=0.5))
legend("topright", c("Female", "Male"), col=c(rgb(0.05,0.05,0.05,0.5),rgb(0.7,0.7,0.7,0.5)),lty=2, lwd=10)

