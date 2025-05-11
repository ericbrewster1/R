# Question 1: Hypothesize what you would expect to see with annual temperature data over the decades 1970s, 1980, 1990s, 2000s, 2010s.
## Due to global warming, I predict the annual temperature will increase steadily over the decades (roughly 2 degrees total over 50 years?)

city1 <- read.csv(
   "http://www.stat.ufl.edu/~winner/data/weather_NOAA/saltlakecity.csv")
attach(city1); names(city1)
head(city1)

## Name of City where year >= 1970 & year < 2020
NAME1 <- NAME[year >= 1970 & year < 2020]   
## Average Temp   "     "   "  "     
TAVG1 <- TAVG[year >= 1970 & year < 2020] 
## Year           "     "   "  "       
YEAR1 <- year[year >= 1970 & year < 2020]        
## City ID (1 for 1st city, 2 for 2nd,...)
CITY_ID1 <- rep(1, length(TAVG1))  

## Combine NAME1, TAVG1, CITY_ID1 with names NAME, TAVG, CITY_ID 
data1 <- data.frame(NAME=NAME1, TAVG=TAVG1, CITY_ID=CITY_ID1, YEAR=YEAR1)
detach(city1)  ## Remove city1 from memory


city2 <- read.csv(
   "http://www.stat.ufl.edu/~winner/data/weather_NOAA/sandiego.csv")
attach(city2); names(city2)
head(city2)

NAME2 <- NAME[year >= 1970 & year < 2020]
TAVG2 <- TAVG[year >= 1970 & year < 2020]
YEAR2 <- year[year >= 1970 & year < 2020]
CITY_ID2 <- rep(2, length(TAVG2))
data2 <- data.frame(NAME=NAME2, TAVG=TAVG2, CITY_ID=CITY_ID2, YEAR=YEAR2)
detach(city2)

city3 <- read.csv(
  "http://www.stat.ufl.edu/~winner/data/weather_NOAA/atlanta.csv")
attach(city3); names(city3)
head(city3)

## Name of City where year >= 1970 & year < 2020
NAME3 <- NAME[year >= 1970 & year < 2020]   
## Average Temp   "     "   "  "     
TAVG3 <- TAVG[year >= 1970 & year < 2020] 
## Year           "     "   "  "       
YEAR3 <- year[year >= 1970 & year < 2020]        
## City ID (1 for 1st city, 2 for 2nd,...)
CITY_ID3 <- rep(3, length(TAVG3))

data3 <- data.frame(NAME=NAME3, TAVG=TAVG3, CITY_ID=CITY_ID3, YEAR=YEAR3)
detach(city3)  ## Remove City3 from memory

city4 <- read.csv(
  "http://www.stat.ufl.edu/~winner/data/weather_NOAA/chicago.csv")
attach(city4); names(city4)
head(city4)

## Name of City where year >= 1970 & year < 2020
NAME4 <- NAME[year >= 1970 & year < 2020]   
## Average Temp   "     "   "  "     
TAVG4 <- TAVG[year >= 1970 & year < 2020] 
## Year           "     "   "  "       
YEAR4 <- year[year >= 1970 & year < 2020]        
## City ID (1 for 1st city, 2 for 2nd,...)
CITY_ID4 <- rep(4, length(TAVG4))  

## Combine NAME4, TAVG4, CITY_ID4 with names NAME, TAVG, CITY_ID 
data4 <- data.frame(NAME=NAME4, TAVG=TAVG4, CITY_ID=CITY_ID4, YEAR=YEAR4)
detach(city4)  ## Remove City4 from memory

city5 <- read.csv(
  "http://www.stat.ufl.edu/~winner/data/weather_NOAA/memphis.csv")
attach(city5); names(city5)
head(city5)

## Name of City where year >= 1970 & year < 2020
NAME5 <- NAME[year >= 1970 & year < 2020]   
## Average Temp   "     "   "  "     
TAVG5 <- TAVG[year >= 1970 & year < 2020] 
## Year           "     "   "  "       
YEAR5 <- year[year >= 1970 & year < 2020]        
## City ID (1 for 1st city, 2 for 2nd,...)
CITY_ID5 <- rep(5, length(TAVG5))  

## Combine NAME5, TAVG5, CITY_ID5 with names NAME, TAVG, CITY_ID 
data5 <- data.frame(NAME=NAME5, TAVG=TAVG5, CITY_ID=CITY_ID5, YEAR=YEAR5)
detach(city5)  ## Remove City5 from memory

city6 <- read.csv(
  "http://www.stat.ufl.edu/~winner/data/weather_NOAA/sanfrancisco.csv")
attach(city6); names(city6)
head(city6)

## Name of City where year >= 1970 & year < 2020
NAME6 <- NAME[year >= 1970 & year < 2020]   
## Average Temp   "     "   "  "     
TAVG6 <- TAVG[year >= 1970 & year < 2020] 
## Year           "     "   "  "       
YEAR6 <- year[year >= 1970 & year < 2020]        
## City ID (1 for 1st city, 2 for 2nd,...)
CITY_ID6 <- rep(6, length(TAVG6))  

## Combine NAME6, TAVG6, CITY_ID6 with names NAME, TAVG, CITY_ID 
data6 <- data.frame(NAME=NAME6, TAVG=TAVG6, CITY_ID=CITY_ID6, YEAR=YEAR6)
detach(city6)  ## Remove City6 from memory

## Stack (row bind) data frames (which have common variable names)
data_all <- rbind(data1,data2,data3,data4,data5,data6)
attach(data_all)
head(data_all); tail(data_all)

## Obtain annual averages by city
annual_ave <- as.numeric(tapply(TAVG, list(YEAR, CITY_ID), mean))
annual_ave 

year_seq <- rep(1970:2019, times=6)  ## Note: you will have 6 cities, not 2
city_seq <- rep(1:6, each=50)        ##  "     "    "    "  "   "      "  "

## Make plots for cities 1 and 2 with smooth lowess (trend) curves added
par(mfrow=c(2,3)) # Plot will have 1 row and 2 columns - yours will be (2,3)
for (i1 in 1:6) { 
  plot(annual_ave[city_seq == i1] ~ year_seq[city_seq == i1],
       main=paste("City ID = ", i1),
       xlab="year", ylab="average annual temperature")
  lines(lowess(annual_ave[city_seq == i1] ~ year_seq[city_seq == i1]))
}

## Create decade from year_seq
decade <- ifelse(year_seq <= 1979 ,1, 
            ifelse(year_seq <= 1989, 2, 
            ifelse(year_seq <= 1999, 3,
            ifelse(year_seq <= 2009, 4, 5))))

## Make decade a factor variable with labels: 1970s,...,2010s
decade <- factor(decade, levels=1:5,
                  labels=c("1970s","1980s","1990s","2000s","2010s"))

## Make city_seq a factor variable with labels
city_seq <- factor(city_seq, levels=1:6, 
             labels=c("SLC", "SD", "ATL", "CHI", "MEM", "SF"))

## Construct a data frame with annual data place in memory
data_annual <- data.frame(year_seq, city_seq, decade, annual_ave)
detach(data_all)
attach(data_annual)

head(data_annual)
tail(data_annual)

## Obtain Decade mean temperatures by city
decade_ave <- as.numeric(tapply(annual_ave, list(decade, city_seq), mean))
decade_ave

decade_seq1 <- rep(1:5, times=6)
city_seq1 <- rep(1:6, each=5)

## Create factor variables for decade and city
decade_seq1.f <- factor(decade_seq1, levels=1:5,
                  labels=c("1970s","1980s","1990s","2000s","2010s"))
city_seq1.f <- factor(city_seq1, levels=1:6, 
             labels=c("SLC", "SD", "ATL", "CHI", "MEM", "SF"))

data_decade <- data.frame(decade_seq1, city_seq1, decade_ave, 
                          decade_seq1.f, city_seq1.f)
detach(data_annual)
attach(data_decade)

data_decade

## Plot decade average by decade with different symbols for cities
## Use the numeric decade_seq1 and city_seq1, not factor vars with .f

par(mfrow=c(1,1))    ## Set back to 1-plot per page
plot(decade_ave ~ decade_seq1, pch=city_seq1, ylim=c(40,110),
     main="Decade Average Temps by City")
legend(1,110, c("SLC", "SD", "ATL", "CHI", "MEM", "SF"), pch=1:6)

mod1 <- aov(decade_ave ~ decade_seq1.f + city_seq1.f)
summary(mod1)
TukeyHSD(mod1, "decade_seq1.f")

# Question 8: Give a brief summary of your findings and compare them with your prediction in part 1
## In my original hypothesis, I predicted a steady increase in average temperature. Looking at the decade averages and the graphs, the total increases ranged from 1.083 to 3.28, so my prediction of two degrees was accurate. The differentials in the Tukey table are mostly positive and less than 1, indicative of a slow increase in average temperature.  
