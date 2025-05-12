# BMI

x <- read.csv("subadultBMI.csv")

# Sex M=male, F=female
# AgeClass P = pre-pubertal, A = adolescent (post-puberty)
# P50 <- BMI (weight/stature^1.33)

# H0: BMI is independent of age class and sex.

# H1: Females have higher BMI than males, but only among adolescents. 

mp <- which(x$Sex == 'M' & x$AgeClass == 'P')
ma <- which(x$Sex == 'M' & x$AgeClass == 'A')
fp <- which(x$Sex == 'F' & x$AgeClass == 'P')
fa <- which(x$Sex == 'F' & x$AgeClass == 'A')

N.mp <- length(mp)
N.ma <- length(ma)
N.fp <- length(fp)
N.fa <- length(fa)

yng <- c(mp, fp)
old <- c(ma, fa)

x[yng,4] <- "Y"
x[old,4] <- "O"
x[,4]

rs.N <- min(c(mp, ma, fp, fa))

F.age <- NULL
F.sex <- NULL
F.sxa <- NULL

for (i in 1:1000) {
  
  dfa <- x[fa,][sample(nrow(x[fa,]),rs.N,replace=FALSE),]
  dfp <- x[fp,][sample(nrow(x[fp,]),rs.N,replace=FALSE),]
  dmp <- x[mp,][sample(nrow(x[mp,]),rs.N,replace=FALSE),]
  
  rs.aov <- as.data.frame(rbind(fa,fp,x[ma,],mp))
  mdl <- anova(aov(rs.aov$P50 ~ rs.aov[,4] * rs.aov$AgeClass))
  F.age [i] <- mdl[1,"F value"]
  F.sex [i] <- mdl[2,"F value"]
  F.sxa [i] <- mdl[3,"F value"]
  
}



F.crit <- qf(0.95,1,28)

sum(F.crit>F.age)/1000          
sum(F.crit>F.sex)/1000          
sum(F.crit>F.sxa)/1000

# Females have higher BMI than males, but only among adolescents

