 # exercise w4.2

# data from Cushny, A. R. and Peebles, A. R. (1905) 
# The action of optical isomers: II hyoscines. 
# The Journal of Physiology 32, 501-510.

library(datasets)
sleep   # extra is additional hours of sleep per night
        # group 2 is a soporific drug treatment  (10 subjects)  
        # group 1 is a control condition (same 10 subjects)

# get bootstrap estimates of the mean of each sample

sleepGroup1 <- sleep[sleep$group == 1, ]
sleepGroup2 <- sleep[sleep$group == 2, ]

group1Boot <- NULL
for(i in 1:1000) {group1Boot[i] <- mean(sample(sleepGroup1$extra, 10, replace = TRUE))}

group2Boot <- NULL
for(i in 1:1000) {group2Boot[i] <- mean(sample(sleepGroup2$extra, 10, replace = TRUE))}

# evaluate results graphically

hist(group1Boot)
hist(group2Boot)

# are the two samples different?
# The most striking difference is that control group is centered between 0 and 1 hours, whereas group 2 is concentrated between 2 and 3.
mean(group1Boot)
mean(group2Boot)
median(group1Boot)
median(group2Boot)
# This is confirmed looking at the means/medians: in general, group 2 gets 1.5 more hours of sleep

# which sample is more variable?
# Will use standard error: SD/sqrt(n)

sd(group1Boot) / sqrt(10)
sd(group2Boot) / sqrt(10)

# Group 2 is more variable than Group 1 (higher standard error)


# These data being paired we can estimate a net effect for the 10 subjects
# subtract drug (group2) from control (group 1)

sleep$net <- sleep$extra[sleep$group == 2] - sleep$extra[sleep$group == 1]
sleep <- sleep[sleep$group == 2, ]

# bootstrap and report a median estimate of the "net" extra sleep

netBoot <- NULL
for(i in 1:1000) {netBoot[i] <- median(sample(sleep$net, 10, replace = TRUE))}
median(netBoot)

# The median net effect is 1.3 hours extra sleep