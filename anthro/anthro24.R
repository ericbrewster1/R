# food proportions in adult and juvenile mangabeys
# Sac: sacoglottis nuts
# Inv: invertebrates
# AM = adult males, AF = adult females, JM = juvenile males, JF = juvenile females
# data from McGraw, W. S., Vick, A. E., & Daegling, D. J. (2011). 
# Sex and age differences in the diet and ingestive behaviors of 
# sooty mangabeys (Cercocebus atys) in the Tai Forest, Ivory Coast. 
# American journal of physical anthropology, 144(1), 140-153.

library(DescTools)

feed <- matrix (c(106,60,425,356,17,14,25,18),nrow=2)

dimnames(feed) <- list(c("Sac","Inv"),c("AM","AF", "JM","JF"))

# Step 1. Perform and interpret a G-test

GTest(feed)
# Not significant evidence to say that the food proportions are different between sex/age


# Step 2. AF are well-sampled, males less so and juveniles have few observations
# If we resample out of the AF data into the N of the other samples
# we can get a sense if any other group (MM, JM, JF) departs significantly from AF
# in terms of proportion of nuts to invertebrates. Such a procedure also can
# also reveal whether observed frequencies in juveniles are an expected outcome of sampling out of 
# the adult female dataset (another test of the null hypothesis). Design, conduct and
# interpret a procedure to see if the juvenile proportions are likely to represent 
# random sampling from the adult data.

afProportions <- feed[, "AF"] / sum(feed[, "AF"])

resampledData <- matrix(NA, nrow = 10000, ncol = 6) 

set.seed(88970542)  
for (i in 1:10000) {
  resampledAM <- sample(c("Sac", "Inv"), size = feed[1, "AM"] + feed[2, "AM"], 
                         replace = TRUE, prob = afProportions)

  resampledJM <- sample(c("Sac", "Inv"), size = feed[1, "JM"] + feed[2, "JM"], 
                         replace = TRUE, prob = afProportions)

  resampledJF <- sample(c("Sac", "Inv"), size = feed[1, "JF"] + feed[2, "JF"], 
                         replace = TRUE, prob = afProportions)

  resampledData[i, 1] <- sum(resampledAM == "Sac") / length(resampledAM)  
  resampledData[i, 2] <- sum(resampledJM == "Sac") / length(resampledJM) 
  resampledData[i, 3] <- sum(resampledJF == "Sac") / length(resampledJF)  
}

observedAM <- feed[, "AM"] / sum(feed[, "AM"])
observedJM <- feed[, "JM"] / sum(feed[, "JM"])
observedJF <- feed[, "JF"] / sum(feed[, "JF"])

pAM <- mean(resampledData[, 1] >= observedAM[1]) 
pJM <- mean(resampledData[, 2] >= observedJM[1])  
pJF <- mean(resampledData[, 3] >= observedJF[1]) 

afProportions
pAM
pJM
pJF

# Juveniles not likely to represent random sampling from the adult data


# Step 3: are the ratios of nuts to bugs the same? Reinterpret the results of 
# your original G-test in terms of your resampling conclusions.

afProportions
observedAM
observedJF
observedJM

# The sample that appears most different to the AF proportions is adult males (more nuts)
# Comparing the resampled data to the original, the group with a significant p-value was AM
# There is sufficient evidence to say that AM varies from AF, but not significant evidence for the juveniles



