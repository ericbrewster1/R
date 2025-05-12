# exercise w7.2  
# economic and life history data from 
# Dama MS (2011) 
# Sex Ratio at Birth and Mortality Rates Are Negatively Related in Humans. 
# PLoS ONE 6(8): e23792. 

library(lmodel2)

x <- read.csv("World.csv")

# Do fertility and infant mortality rate (IMR) have the same relationship 
# across the world development scheme (1st , 2nd and 3rd Worlds)?

# Step 1. Are fertility and infant mortality correlated within each
#         of the three world levels? If any are not, 
#         EXCLUDE from further analysis

firstWorld <- x[x$World == 1, ]
secondWorld <- x[x$World == 2, ]
thirdWorld <- x[x$World == 3, ]

cor.test(firstWorld$Fertility, firstWorld$IMR)
cor.test(secondWorld$Fertility, secondWorld$IMR)
cor.test(thirdWorld$Fertility, thirdWorld$IMR)

# the first world will be excluded from further analysis - not correlated at significant level

x <- x[x$World != 1, ]
x <- x[order(x$World), ]

# Step 2. Now conduct an ANCOVA-like procedure based on
#         a major axis (MA) regression. Evaluate differences
#         among slopes. If significance is not found, proceed to test
#         for differences among intercepts.

secondma <- lmodel2(IMR~Fertility,data=secondWorld,nperm=999)
thirdma <- lmodel2(IMR~Fertility,data=thirdWorld,nperm=999)


mcint <- NULL

empsecond <- secondma$regression.results[2,2]
empthird <- thirdma$regression.results[2,2]
intdiff <- empthird - empsecond

for (i in 1:1000) {
  
  mcall <- sample(nrow(x),length(x$World),replace=FALSE)
  second <- mcall[1:32]
  third <- mcall[33:length(mcall)]
  secondrma <- lmodel2(x$IMR[second]~x$Fertility[second],
                  range.x="relative", range.y="relative",nperm=1) # suppressing RMA message
  thirdrma <- lmodel2(x$IMR[third]~x$Fertility[third],
                  range.x="relative", range.y="relative",nperm=1)
  mcint[i] <- thirdrma$regression.results[2,2] - 
    secondrma$regression.results[2,2]
  
}

sum (mcint > intdiff) / length(mcint)  # 1-tailed  


twoN <- x[x$World=="2",] 
threeN <- x[x$World=="3",] 

bsint <- NULL

for (i in 1:1000) {
  
  
  twogtr <- sample(nrow(twoN), 32, replace=TRUE)
  threegtr <- sample(nrow(threeN), 110, replace=TRUE)
  
  tworma <- lmodel2(x$IMR[twogtr]~x$Fertility[twogtr],
                  range.x="relative", range.y="relative",nperm=1) # supressing RMA message
  threerma <- lmodel2(x$IMR[threegtr]~x$Fertility[threegtr],
                  range.x="relative", range.y="relative",nperm=1)
  bsint[i] <- tworma$regression.results[2,3] - 
    threerma$regression.results[2,3]
  
}

length(bsint[bsint > 0]) / 1000




# Step 3. Offer an interpretation of results. 
#         Well-designed graphical summaries will suffice.

boxplot(x$IMR~x$World)
boxplot(x$Fertility~x$World)
