# Final exam question 4
# Foraging returns in Hadza fathers and mothers 
# fid = father identifier	
# mid	= mother identifier
# kcal = child returns (not used)	
# kcalm	= mothers foraging return
# kcalf	= fathers foraging return
# sex	= child sex code (not used)
# age = child's age code (not used) 
# agem = mother's age	
# agef = father's age

# Marlowe's data from Borgerhoff-Mulder et al.2009
# Science 326 (5953), 682-688 DOI: 10.1126/science.1178336

x <- read.csv("hadza_hg_returns.csv")
# NOTE: these data are not analysis ready!
# use 7.1

# H0: age has no impact on foraging returns in mothers and fathers

# H1: mothers foraging returns decline less with age

x <- x[, !names(x) %in% c("kcal", "age", "sex")] # not needed columns

x <- x[!(is.na(x$kcalm) & is.na(x$kcalf)), ] # if both kcal are NA, row cannot be used

x <- x[order(x$fid), ] 
# looking at this view (and using duplicated()), there are identical rows

x <- x[!duplicated(x), ]
# Now down to 118 rows

# father's foraging patterns

father <- x[!(is.na(x$fid) | is.na(x$kcalf) | is.na(x$agef)), ]
father <- father[!duplicated(father$fid), ]
father <- father[, c("fid", "kcalf", "agef")]

# mother's foraging patterns

mother <- x[!(is.na(x$mid) | is.na(x$kcalm) | is.na(x$agem)), ]
mother <- mother[!duplicated(mother$mid), ]
mother <- mother[, c("mid", "kcalm", "agem")]


# Now have the data for mother and father foraging patterns

empFather <- as.numeric(lm(father$kcalf~father$agef)$coefficients)
empMother <- as.numeric(lm(mother$kcalm~mother$agem)$coefficients)

s0 <- numeric (1000)
s1 <- numeric (1000)
b0 <- numeric (1000)
b1 <- numeric (1000)

for (i in 1:1000) {
  
  fatherSample <- father[sample(nrow(father),40,replace=TRUE),c(2,3)]
  motherSample <- mother[sample(nrow(mother),63,replace=TRUE),c(2,3)]
  r0 <- lm(fatherSample$kcalf~fatherSample$agef)
  r1 <- lm(motherSample$kcalm~motherSample$agem)
  s0[i] <- r0$coefficients[2]
  s1[i] <- r1$coefficients[2]
  b0[i] <- r0$coefficients[1]
  b1[i] <- r1$coefficients[1]
  
}

par(mfrow=c(2,1))
hist(s0,xlim=c(-50,110),breaks=10)
hist(s1,xlim=c(-50,110),breaks=10)

quantile(s0,c(0.025,0.975))  # no slope, CI includes zero 
quantile(s1,c(0.025,0.975))  # CI > 0, positive slope


hist(s0)
abline(v=empFather[2],col="red")

hist(s1)
abline(v=empMother[2],col="blue")

ps0 <- ecdf(s0)  # cumulative probability function
ps0(empFather[2]) 

ps1 <- ecdf(s1)
ps1(empMother[2])


pb0 <- ecdf(b0)(empFather[1]) # 0.493

pb1 <- ecdf(b1)(empMother[1]) # 0.522

plot (s0,b0,xlim=c(-40,110),ylim=c(-500,500),cex=0.5)
points(s1,b1,col="red",cex=0.5)


# Mothers foraging returns decline less with age (positive slope when resampled)
