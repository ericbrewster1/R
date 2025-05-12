# exercise w7.1:  resampling for ANCOVA

# does antibody level vary as a function of age?
# is this relationship the same if children are infected with malaria?

# step 1 retrieve data

library(ISwR)   # install from CRAN mirror if needed

malaria    # ?malaria for information on data

# step 2 modify object structure as needed for analysis

malaria$mal <- as.factor(malaria$mal)
malariaYes <- malaria[malaria$mal == 1, ]
malariaNo <- malaria[malaria$mal == 0, ]

# step 3 create resampled distribution of slopes and intercepts
#        use malaria status (mal) as your grouping variable
#        analyze antibodies (Y) relative to age (X) 
#        in those with and without malaria separately

results <- list()

for (group in levels(malaria$mal)) {
  groupData <- malaria[malaria$mal == group, ]
  
  slopes <- numeric(1000)
  intercepts <- numeric(1000)
  
  for (i in 1:1000) {
    sampleData <- groupData[sample(1:nrow(groupData), replace = TRUE), ]
    model <- lm(ab ~ age, data = sampleData)
    slopes[i] <- coef(model)[2]
    intercepts[i] <- coef(model)[1]
  }
  
  results[[group]] <- list(slopes = slopes, intercepts = intercepts)
}

# step 4 decide if slopes are significant (i.e., not zero)

quantile(results[[1]]$slopes, c(0.025, 0.975))
quantile(results[[2]]$slopes, c(0.025, 0.975))
# The slopes for people without malaria are significant (0 is not in the 95% CI)
# The slopes for people with malaria are not significant (0 is in the 95% CI)


# step 5 determine if slopes are significantly different between groups

t.test(results[[1]]$slopes, results[[2]]$slopes)$p.value
# Yes, the slopes are significantly different between groups, as the p-value is far below .05


# step 6 if slopes are not different, examine intercepts between groups
# The slopes are significantly different


# step 7 interpret the results
# Hint: A graphic is worth a thousand words!

reg1<-lm(ab~age,data=malariaYes)
reg2<-lm(ab~age,data=malariaNo) 

plot(ab~age, data=malaria, type='n')
points(malariaYes$age,malariaYes$ab, pch=20)
points(malariaNo$age,malariaNo$ab, pch=1)
abline(reg1, lty=1)
abline(reg2, lty=2)
legend("topleft", c("Malaria","No Malaria"), lty=c(1,2), pch=c(20,1) )

# As seen in the graph, children with malaria generally have a near-zero level of antibodies
# However, children without malaria have a move variable level of antibodies (greater slope, antibodies increases more with age)
