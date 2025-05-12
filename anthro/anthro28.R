# Are lynx numbers associated with sunspot activity?

library(Sleuth2)

ex1513

# H0: there is no association between lynx population and sunspots

# H1: Over decade intervals, changes in lynx numbers and sunspot activity correspond 

library(tseries)
library(zoo)

lynxTS <- zoo(ex1513$Lynx, ex1513$Year)
sunspotTS <- zoo(ex1513$Spots, ex1513$Year)

acf(lynxTS)
pacf(lynxTS)
Box.test(lynxTS, lag = 10)

acf(sunspotTS)
pacf(sunspotTS)
Box.test(sunspotTS, lag = 10)

deltaLynx10 <- diff(lynxTS, lag=10)
deltaSpots10 <- diff(sunspotTS, lag=10)

# removes 1911 and 1924 - values are 0
deltaSpots10 <- deltaSpots10[deltaSpots10 != 0]

L10 <- factor(sign(deltaLynx10))
S10 <- factor(sign(deltaSpots10))

runs.test(L10)
runs.test(S10)

plot(lynxTS)
lines(sunspotTS)

# Evidence suggests H1: lynx and sunspots are correlated
# Box-Pierce tests for both time series are p-value < 2.2e-16: significant autocorrelation in both
# Both runs tests for decade lags produced very low p-values: observations grouped together
# Not too clear from the graph, but looks like sunspots seems to go up with increased lynx populations and vice versa.


