# Final question 3
# HairEyeColor R dataset: Males
#       Eye
# Hair    Brown Blue Hazel Green
# Black    32   11    10     3
# Brown    53   50    25    15
# Red      10   10     7     7
# Blond     3   30     5     8

# H0: eye color and hair color are independently distributed


# H1: eye color is distributed similarly in men with red versus brown hair

x <- HairEyeColor[,, "Male"]
chisq.test(x)

x <- x[c("Red", "Brown"), ]
chisq.test(x)
fisher.test(x)

# Using chi-squared test, first p-value is very low: eye and hair color not independently distributed in entire sample
# Breaking down into just red and brown hair, using chi-squared and Fisher's exact give a p-value ~ 0.38
# Not significant evidence to say eye color is  distributed different between red and brown hair
