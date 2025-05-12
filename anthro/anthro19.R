# exercise w8.2  balance a nested design

# Data from Tubb, A., Parker, A. J., & Nickless, G. (1980), 
# The analysis of Romano-British pottery by atomic absorption spectrophotometry. 
# Archaeometry, 22(2), 153-171. 
# Classification of regions/samples modified to fit problem
# see table in Baxter (2003) for complete dataset

# % Aluminum Oxide in ceramic samples from 2 regions

# Step 1. Get the data

x <- read.csv("chem ceramic.csv")

# Step 2. identify nature of unbalanced design
#         nesting samples within regions
#         ignore Kilns as a factor

nest <- anova(aov(x$Al203~x$Region/x$Sample))
print(nest)

table(x$Region)
table(x$Sample)

# Step 3. carry out a resampling procedure to create a balanced design
#         Avoid pseudoreplication!
#         Ignore Kilns again

set.seed(123)

reg1Indices <- sample(which(x$Region == 1), 15)
reg1Samples <- x[reg1Indices, ]

reg2Indices <- sample(which(x$Region == 2), 15)
reg2Samples <- x[reg2Indices, ]

balancedData <- rbind(reg1Samples, reg2Samples)
anova(aov(balancedData$Al203~balancedData$Region/balancedData$Sample))

# Step 4: Do a one-way anova using Kilns as factors
#         1-way anovas do NOT need equal N among groups
 
anova(aov(Al203 ~ Kiln, data = x))
anova(aov(Al203 ~ Kiln + Region + Sample, data = x))


# Step 5: How does nested result help us understand 1-way result?

# The nested result shows that region and samples nested within region are very significantly correlated to Al203 level
# In the one way anova, kiln does not contribute significantly to the variability of Al203

