# w11.1   Using a time series
# proportion of females in population 1960-2017
# Source: World Bank
# see Hannah Ritchie and Max Roser (2019) - "Gender Ratio". 
# Published online at OurWorldInData.org. 
# Retrieved from: 'https://ourworldindata.org/gender-ratio' 

library (zoo) 
library (tseries)
x <-read.csv("popfemale.csv")

# Step 1: plot female population versus year for 1) Belarus, 2) Angola

plot(x[4][x$Entity == "Belarus", ], x[3][x$Entity == "Belarus", ], type = "l")
plot(x[4][x$Entity == "Angola", ], x[3][x$Entity == "Angola", ], type = "l")

# Step 2: convert objects to time series

belarusMLTS <- zoo(x[4][x$Entity == "Belarus", ], x[3][x$Entity == "Belarus", ])
angolaMLTS <- zoo(x[4][x$Entity == "Angola", ], x[3][x$Entity == "Angola", ])
combo <- cbind(belarusMLTS, angolaMLTS)

# Step 3: examine autocorrelation

acf(belarusMLTS)
Box.test(belarusMLTS)

acf(angolaMLTS)
Box.test(angolaMLTS)

# p-value very low for both Belarus and Angola - evidence to claim that female populations are time-dependent

# Step 4: compare trends using a runs test for yearly and decade changes

deltam <- diff(belarusMLTS, lag=1)  # iterate through different lags = 1
updown <- factor(sign(deltam))
runs.test(updown)

deltam <- diff(belarusMLTS, lag=10)  # iterate through different lags = 10
updown <- factor(sign(deltam))
runs.test(updown)

deltam <- diff(angolaMLTS, lag=1)  # iterate through different lags = 1
updown <- factor(sign(deltam))
runs.test(updown)

deltam <- diff(angolaMLTS, lag=10)  # iterate through different lags = 10
updown <- factor(sign(deltam))
runs.test(updown)

# Both statistics are negative and highly significant - not many runs (can be seen in the graph - 1/2 predominant run/s)


# Step 5: is temporal dependence similar between countries?

# The runs tests (in lags 1 and 10) for both countries have a stat near -6 and a very significant p-value
# In the graphs, both countries saw a decrease in female population over the time period
