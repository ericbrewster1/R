# exercise w5.1
# type 1 errors for misbehaving distributions

# Step 1 create baseline distributions for tests

# a normal distribution

OK <- rnorm(100000, mean=20, sd=1)                 

# normal as OK, higher variance
HI <- rnorm(100000, mean=20, sd=3)            

# not normal, but similar variance to OK
NN <- runif(100000, min = quantile(OK,0.04), max = quantile(OK, 0.96))  

# Step 2. Demonstrate type 1 error from conventional t-test
# for 50000 trials
# get 2 samples of 50 observations each from object OK
# run t-test comparing samples
# count significant tests 
# report type 1 error

significantCount <- 0

for(i in 1:50000) {
  sample1 <- sample(OK, 50)
  sample2 <- sample(OK, 50)
  if(t.test(sample1, sample2)$p.value < 0.05) {
    significantCount <- significantCount + 1
  }
}

type1error <- significantCount / 50000
type1error


# Step 3. Demonstrate type 1 error for samples of unequal variances
# for 50000 trials
# get 1 sample of 30 observations from object OK
# get 1 sample of 30 observations from object HI
# run t-test comparing samples (set var.equal=TRUE)
# count significant tests 
# report type 1 error

significantCount <- 0

for(i in 1:50000) {
  sample1 <- sample(OK, 30)
  sample2 <- sample(HI, 30)
  tTest <- t.test(sample1, sample2, var.equal = TRUE)
  if(tTest$p.value < 0.05) {
    significantCount <- significantCount + 1
  }
}

type1error <- significantCount / 50000
type1error


# Step 4. Proceed as Step 3 but set var.equal=FALSE in t.test().
# report type 1 error

significantCount <- 0

for(i in 1:50000) {
  sample1 <- sample(OK, 30)
  sample2 <- sample(HI, 30)
  tTest <- t.test(sample1, sample2, var.equal = FALSE)
  if(tTest$p.value < 0.05) {
    significantCount <- significantCount + 1
  }
}

type1error <- significantCount / 50000
type1error


# Step 5. Demonstrate type 1 error when a sample is not normal
# for 50000 trials
# get 1 sample of 30 observations from object OK
# get 1 sample of 30 observations from object NN
# run t-test comparing samples 
# count significant tests 
# report type 1 error

significantCount <- 0

for(i in 1:50000) {
  sample1 <- sample(OK, 50)
  sample2 <- sample(NN, 50)
  if(t.test(sample1, sample2)$p.value < 0.05) {
    significantCount <- significantCount + 1
  }
}

type1error <- significantCount / 50000
type1error

# Step 6. Given that you are expecting type 1 errors at about 5% rate
# how problematic is relying on the t-test 
# if distributional assumptions are violated? 

# Each of the tests above yield an error rate very close to 5%, despite any violated assumptions.
# Therefore, it does not seem problematic to trust the t-test when assumptions are violated.

