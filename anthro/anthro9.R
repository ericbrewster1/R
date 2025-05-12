# exercise w4.3
# Alouatta palliata brain size: Isler 2008 JHE
# variable is endocranial volume in cc

females <- c(56.07,53.80,40.00,45.00,48.50,44.00,44.50,44.00,
             48.50,49.00,50.00,48.00,53.20)
males <- c(52.00,56.79,58.71,50.50,51.00,42.50,50.50,54.00,
           49.50,53.00,49.50,54.00,51.50,49.00,48.50,51.00,
           51.00,57.75)

# Step 1 calculate the difference in observed means (M - F)
(observedDiff <- mean(males) - mean(females))

# Step 2 pool the data
dataPool <- data.frame(gender = c(rep("Female", 13), rep("Male", 18)), 
                       value = c(females, males))

# Step 3 bootstrap to original n, creating "dummy" M, F samples 
femaleBoot <- NULL
for(i in 1:1000) {femaleBoot[i] <- mean(sample(dataPool$value[dataPool$gender == "Female"], 13, replace = TRUE))}

maleBoot <- NULL
for(i in 1:1000) {maleBoot[i] <- mean(sample(dataPool$value[dataPool$gender == "Male"], 18, replace = TRUE))}

# Step 4 get mean difference of M - F for each iteration
differenceBoot <- maleBoot - femaleBoot

# Step 5 count how often observed difference exceeds resampled difference 
sum(observedDiff > differenceBoot)

# Step 6 Express result as a probability
sum(observedDiff > differenceBoot) / 1000

# Step 7 Repeat steps 3-6 but using a monte carlo procedure
femaleMC <- NULL
for(i in 1:1000) {femaleMC[i] <- rnorm(1, mean = mean(females), sd = sd(females))}

maleMC <- NULL
for(i in 1:1000) {maleMC[i] <- rnorm(1, mean = mean(males), sd = sd(males))}

differenceMC <- maleMC - femaleMC

sum(observedDiff > differenceMC)

sum(observedDiff > differenceMC) / 1000

# Step 8 Do the results obtained by the two procedures agree?

# The results from the two procedures tend to be around half the observations, though the monte carlo and bootstrap do differ by as many as ~30 observations


