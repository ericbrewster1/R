 # Create two novel functions using function()
# coefficient of variation (CV) and mode do not have functions in base R
# Since the CV and mode are common descriptive statistics,
# you should anticipate that others have solved this problem before you
# take advantage of online resources!

# Step 1. write a function to find the mode of a vector
modeVec<-function(x) {
  uniqueValues <- unique(x)
  uniqueValues[which.max(tabulate(match(x, uniqueValues)))]
}

# inspired by a solution on stack overflow: https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
# unique(x) creates a vector of the distinct numbers in the vector
# The second line 

# Step 2. demonstrate that the function works correctly
test1 <- c(1, 2, 3, 3, 3)
modeVec(test1)
test2 <- c(0, 0, 0, 0, 3, 3, 3, 5, 7, 9, 45, 689, 0)
modeVec(test2)

# Step 3. write a function to find the coefficient of variation
coVar<-function(x) {
  sd(x) / mean(x) * 100
}

# Step 4. demonstrate that the function works correctly

test3 <- c(0, 1, 2, 3, 4)
coVar(test3)
test4 <- c(1, 4, 7, 10, 20, 25)
coVar(test4)

##############
# NOTE: demonstrating a function here means making 
#       the function its own unique object
##############