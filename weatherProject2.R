# Part 1: Choice and Climate Change

# For my survey, I formatted my results similar to the risk gamble data: two columns, numbers 1 and 2 representing sure choices and risky choices. 
# The scenario was posed as "Imagine that Florida is preparing for warming-induced rising sea levels, which will destroy 5000 homes.
# Two programs have been proposed to combat the rising sea levels.

# My question posed as a gain was "If Program A is adopted, 1250 of the homes can be saved.
# If Program B is adopted, there is a 1/4 chance that 5000 homes would be saved, and a 3/4 chance no homes would be saved.
# My question posed as a loss was "If program A is adopted, 3750 homes will be lost.
# If Program B is adopted, there is 1/4 chance that no homes be lost, and a 3/4 chance all the homes would be lost. 
# I mixed these in with choices about varying damage to homes, amount of sea level rise, timing of home damage over the next decade, and different locations.

risk <- read.csv("riskchart.csv")
attach(risk); names(risk)

gain <- factor(gain, levels=1:2, labels=c("Sure_G","Risk_G"))
loss <- factor(loss, levels=1:2, labels=c("Sure_L","Risk_L"))

(risk.table <- table(gain,loss))

margin.table(risk.table,1) # sure gain or risky gain?
margin.table(risk.table,2) # sure loss or risky loss?

risk.table/sum(risk.table) # proportion of all responses

prop.table(risk.table,1)  # loss within gain

prop.table(risk.table,2)  # gain within loss

t(prop.table(risk.table,1))

par(mfrow=c(1,2))   # 1 row, 2 columns
barplot(t(prop.table(risk.table,1)),beside=T,
        legend=colnames(risk.table),ylim=c(0,1.4), 
        main="Grouped Bar Plot")

barplot(t(prop.table(risk.table,1)),beside=F,
        legend=colnames(risk.table),
        main="Stacked Bar Plot", ylim=c(0,1.60))
detach(risk)

# Summary of my results
# As I expected, the results of my survey aligned closely with the risk in gambling R file.
# My respondents favored sure gains and risky losses, with sure gain and risky loss as the most common response at about 64 percent.
# The difference was the most stark in the sure gains, as almost 90% of sure gain selectors also opted for a risky loss.
# Perhaps the difference is most evident in the plots, as the risky losses tower over the sure losses.
# The lower odds (here, 1 in 4) of obtaining the risky gain prompt most respondents to choose the safe option.
# Conversely, respondents presumably choose the risky gain because the difference between 3750 and 5000 homes lost does not seem stark.

# Part 2: Patterns of Cyclones Around Antarctica

cyclone <- read.csv("http://www.stat.ufl.edu/~winner/data/cyclone.csv")
attach(cyclone); names(cyclone)

latLong <- factor(latLong, levels=1:3, labels=c("40-49", "50-59", "60-69"))
season <- factor(season, levels=1:4, labels=c("Fall", "Winter", "Spring", "Summer"))

cyclone.table <- table(latLong, season)

prop.table(cyclone.table, 1)

par(mfrow=c(1,2))   # 1 row and 2 columns of plots
barplot(t(prop.table(cyclone.table,1)),beside=T,
        legend=colnames(cyclone.table),ylim=c(0,0.8), 
        main="Grouped Bar Plot")

barplot(t(prop.table(cyclone.table,1)),beside=F,
        legend=colnames(cyclone.table),
        main="Stacked Bar Plot", ylim=c(0,2))

X2.cyclone <- chisq.test(cyclone.table)
X2.cyclone
X2.cyclone$stdres

detach(cyclone)

# Summary of my results
# Given the results of the chi-squared test, the very low p-value and x^2 value of 71 indicates that there exists a stark difference in the incidence of cyclones by latitude and season.
# This is verified by the plots and the contingency table, especially in the summer season.
# In summer in 40-49 latitude, cyclone incidence is relatively similar among all seasons, with less than a 10% difference in proportion between the highest (Winter) and lowest (Spring) cyclone incidences.
# Compared to the 50-59, however, the rates of cyclone incidence vary greatly, as summer commands almost 39% of the cyclones in that latitude range, 17% more than the next closest proportion (Winter).
# The region/season combinations differing most from expected occur in the 40-49 latitude range, especially in summer.
# In 40-49's summer, almost 7 fewer cyclones occur than expected. Conversely, the fall and winter in 40-49 yield around 4 and 5 more cyclones than expected, respectively.
# The other stark difference occurs in 50-59's summer, as almost 5 more cyclones occurred than expected. 

