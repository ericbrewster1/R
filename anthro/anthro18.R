# exercise w8.1
# two way anova
# 

library(ISwR)   # install from CRAN mirror if needed

malaria 

# Step 1. figure out the median age of the sample

median(malaria$age)

# Step 2. Partition age into 2 classes based on step 1.
#         Now find the N of the factor combinations of age and malaria status

malaria$ageClass <- ifelse(malaria$age < 9, "Young", "Old")
table(malaria$ageClass, malaria$mal)

length(malaria$ab[malaria$mal==0 & malaria$ageClass=="Young"])
length(malaria$ab[malaria$mal==0 & malaria$ageClass=="Old"])
length(malaria$ab[malaria$mal==1 & malaria$ageClass=="Young"])
length(malaria$ab[malaria$mal==1 & malaria$ageClass=="Old"])

# Step 3. Run a 2-way anova with equal N (this will require resampling)
#         Treat this as a Model I anova:
#         The denominator MS for main effects and interaction F-ratios
#         is the residual (error) MS
#         

set.seed(88970542)
youngNonMalariaSample <- malaria[malaria$mal == 0 & malaria$ageClass == "Young", ][sample(1:31, 12), ]
oldNonMalariaSample <- malaria[malaria$mal == 0 & malaria$ageClass == "Old", ][sample(1:42, 12), ]
youngMalariaSample <- malaria[malaria$mal == 1 & malaria$ageClass == "Young", ][sample(1:15, 12), ]
oldMalariaSample <- malaria[malaria$mal == 1 & malaria$ageClass == "Old", ][sample(1:12, 12), ]
balancedData <- rbind(youngNonMalariaSample, oldNonMalariaSample, youngMalariaSample, oldMalariaSample)

anovaResults <- aov(ab ~ ageClass * mal, data = balancedData)
summary(anovaResults)


# Step 4. Interpret your results

# Malaria presence is significantly correlated to malaria antibody levels at 0.05 level, whereas ageClass is nearly signifcant