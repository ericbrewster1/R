#  exercise w5.2
#  Resampling a one-way anova
#  Taxon is trivial name of hominin species
#  Asfc is microwear complexity ("roughness") NOT USED IN THIS ASSIGNMENT
#  Lsar is microwear anisotropy ("directionality")

x <- read.csv("hominin microwear fabrics.csv")

# check normality of data for Lsar

shapiro.test(x$Lsar)
# Shapiro test yields a p-value < 0.05, indicating that the data is not normal

# check species for equivalent variance in Lsar

x$Taxon <- as.factor(x$Taxon)
leveneTest(x$Lsar ~ x$Taxon)

fligner.test((x$Lsar~x$Taxon))
# The levene and fligner tests return a p-value above 0.05, prompting us to accept null (variances are similar)
# Worth noting that the p-values for both tests, especially Fligner, is very close to 0.05


# run ANOVA on original data, Lsar by Taxon
# given distributional assumptions are not met, run a resampled ANOVA 
# shuffle factors (i.e., resample Taxon without replacement)
# leave Lsar as is
# create a distribution of F-ratios
# compare original F-ratio to the resampled F-distribution and evaluate

anova(lm(x$Lsar~x$Taxon))
(originalF <- anova(lm(x$Lsar~x$Taxon))$F[1])

fRatiosResample <- numeric(1000)
for(i in 1:1000) {
  taxonRep <- sample(x$Taxon)
  shuffleAnova <- anova(lm(x$Lsar ~ taxonRep))
  fRatiosResample[i] <- shuffleAnova$F[1]
}

mean(fRatiosResample >= originalF)
mean(fRatiosResample)

# The original F-ratio is far higher than the average of the F-ratios when resampled
# Also, the resampled F-ratio tends to be higher than the original about 5 percent of the time