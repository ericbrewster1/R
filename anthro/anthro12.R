# exercise 5.4 two-sample Monte Carlo in lieu of t.test
# gestation time in random samples of girls born in Wyoming & Alaska

# HO: no difference in birthweight/gestation between states
# naive H1: higher birthweight (g), longer gestation (w) in Alaska

# Step 1: get the data

library(resampledata)

x <- Girls2004

# Step 2: use a monte carlo procedure to test for 
# difference in means for birthweight
# Note: your alternative hypothesis is a "one-tailed" test
# evaluate your probability accordingly

m1 <- (x$Weight[x$State=="WY"])
m2 <- (x$Weight[x$State=="AK"])
pool <- c(m1, m2)
dum1 <- numeric(10000)
dum2 <- numeric(10000)
signdif<-(mean(m1)-mean(m2))

for(i in 1:10000){
  
  mc <- sample(pool,80,replace=FALSE)
  dum1[i] <- mean(mc[1:40])
  dum2[i] <- mean(mc[41:80])
  
}

mcdif<-dum1-dum2 
absdif<-abs(dum1-dum2)

hist(mcdif,main="monte carlo mean differences",xlab="dummy1-dummy2",xlim=c(-4,4))
abline(v=signdif,col="red")

# Step 3: use a monte carlo procedure to test for 
# difference in means for gestation

m1 <- (x$Gestation[x$State=="WY"])
m2 <- (x$Gestation[x$State=="AK"])
pool <- c(m1, m2)
dum1 <- numeric(10000)
dum2 <- numeric(10000)
signdif<-(mean(m1)-mean(m2))

for(i in 1:10000){
  
  mc <- sample(pool,80,replace=FALSE)
  dum1[i] <- mean(mc[1:40])
  dum2[i] <- mean(mc[41:80])
  
}

mcdif<-dum1-dum2 
absdif<-abs(dum1-dum2)

hist(mcdif,main="monte carlo mean differences",xlab="dummy1-dummy2",xlim=c(-4,4))
abline(v=signdif,col="red")

# Step 4: compare your monte carlo P to 
# 1) t-test, 2) wilcoxon test probabilities

t.test(Weight ~ State, data = x, alternative = "greater")$p.value
wilcox.test(Weight ~ State, data = x, alternative = "greater")$p.value

t.test(Gestation ~ State, data = x, alternative = "greater")$p.value
wilcox.test(Gestation ~ State, data = x, alternative = "greater")$p.value
