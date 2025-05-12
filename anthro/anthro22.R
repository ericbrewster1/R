# w10.1  resample 2 x 2 table
# social rank in chimpanzees and other primates
# data from Reader, S. M., & Laland, K. N. (2001). 
# Primate innovation: sex, age and social rank differences. 
# International journal of primatology, 22(5), 787-805.

# Does social rank influence innovation differently in
# chimpanzees versus other nonhuman primates?

high<-c(7,5)
low<-c(22,8)
tbl<-rbind(high,low)
dimnames(tbl) <- list(c("high","low"),c("chimps","others"))

# Step 1. calculate an odds ratio for the original table

(OR <- (tbl[1,1]*tbl[2,2])/(tbl[1,2]*tbl[2,1]))
# 0.5090909


# Step 2. resample the table, maintaining marginal row and column totals
# generate a large sample of resampled odds ratios
#      	chimps others	
# high	7	     5
# low 	22     8

mc1= 7+22
mc2= 5+8
mr1= 7+5
mr2= 22+8
Pca <- c(rep(1,7),rep(0,22))  # 0=low
rsO<-NULL
for (i in 1:1000){
  x<-sample(Pca,mc1,replace=TRUE)
  n11<-length(x[x==1])
  n21<-length(x[x==0])
  n12<-mr1-n11
  n22<-mr2-n21
  rsO[i] <- (n11*n22) / (n12*n21)
  
}
hist(rsO,breaks=30)
abline(v=OR,lty=2,lwd=2,col="maroon")



# Step 3. calculate a confidence interval for the resampled odds ratios
# a CI including 1 suggests nonsignificant ratio of probabilities 

min(rsO)
max(rsO)
quantile(rsO, c(0.025,0.975))

# The 95% confidence interval of the resampled odds ratios contains 1, suggesting nonsignificant ratio of probabilities


# Step 4. Run a chisquare test on the original data. 
# Does this result agree with your conclusion concerning the odds ratio?

chisq.test(tbl)

# The chi-square test on original data yields a p-value of 0.5616
# This p-value (far above the significance threshold) agrees with the conclusion that the odds ratio is not significant