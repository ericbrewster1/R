# dsa 9.2  spatial autocorrelation via the Mantel Test
# biconical artifact densities at 10 locations in Lower Miss. Valley
# Pierce, Christopher 1998 Theory, measurement, and explanation: 
# variable shapes in Poverty Point Objects. 
# In Unit Issues in Archaeology 
# edited by Ann F. Ramenofsky and Anastasia Steffen
# pp.163-190. Utah University Press, Salt Lake City.

x <- read.csv("artifact density.csv")

# Eloc, Nloc are coded coordinates 
# Count is number of biconical artifacts
# Total is the total number of all artifacts

# Step 1. Create a relative frequency variable: counts/total

x$freq <- x$Count / x$Total
x$freq

# Step 2. create dissimilarity matrices

(elocDis <- dist(x$Eloc))
(nlocDis <- dist(x$Nloc))
(countDis <- dist(x$Count))
(totalDis <- dist(x$Total))
(freqDis <- dist(x$freq))

# Step 3. Run and interpret Mantel test

space <- dist(cbind(x$Eloc, x$Nloc))
space
test <- mantel.rtest(space, space, nrepet=999)
test
plot(test)

# Significant p-value - space is correlated with itself - evidence that eloc/nloc are correlated


