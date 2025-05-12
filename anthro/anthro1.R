# w2.1 write, edit, and save script.
# Data from Abbott, 1950
# Transactions of the Royal Society of Tropical Medicine and Hygiene
# vol 43 (5): 477-492

# consult the file Abbott Table 1 for the data

# Step 1. name and make a vector of categories (boys, girls, men, women)

vecNames <- c("boys", "girls", "men", "women")

# Step 2. name and make a vector of the number of people in each category for step 1 

vecNumPeople <- c(185, 154, 92, 122)

# Step 3. name and make a vector of "TEETH carious" counts for the 
#         people categories in step 1 

vecTeeth <- c(15, 12, 19, 37)

# Step 4. name and make a vector that contains 
#         the percentage of caries in each people category

vecPercent <- (vecTeeth / vecNumPeople) * 100

# Step 5. combine all vectors into a single object (data frame)

abbottDF <- data.frame(vecNames, vecNumPeople, vecTeeth, vecPercent)

# Step 6. verify that your new object is a data frame

class(abbottDF)

# Step 7. verify class of original vectors with str()
#         then verify class in the dataframe with str()
#         has anything changed?

str(vecNames)
str(vecNumPeople)
str(vecTeeth)
str(vecPercent)
str(abbottDF)

# The classes have not changed when inserted into a data frame

# Step 8. get the sum of caries across all categories
#         get the sum of total people across all categories
#         Try this with your recently made data frame
#         Proceed to step 9 if this didn't work

(sumCaries <- sum(vecTeeth))
(sumPeople <- sum(vecNumPeople))
## or sum(abbottDF$vecNumPeople)

# Step 9. Change class from "factor" to "numeric" by
#         df$caries <- as.numeric(as.character(df$caries))
#         where "df" is the name of your data frame and 
#         "caries" is the name of your caries vector
#         do this for your other numeric vector as well in your data frame

abbottDF$vecTeeth <- as.numeric(as.character(abbottDF$vecTeeth))
abbottDF$vecPercent <- as.numeric(as.character(abbottDF$vecPercent))
abbottDF$vecNumPeople <- as.numeric(as.character(abbottDF$vecNumPeople))

# Step 10. Now repeat step 8, and calculate the percentage caries overall

(sumCaries <- sum(vecTeeth))
(sumPeople <- sum(vecNumPeople))
(vecPercent <- (vecTeeth / vecNumPeople) * 100)
(vecCariesOverall <- sumCaries/sumPeople)


# Step 11. Save this file as "Mylastname_w2.1_f24.R" and submit

######### IMPORTANT NOTE ###########

# you should convince yourself that with some online help you 
# would have been able to solve any problem with the data 
# frame conversion on your own. Generate the error again, then 
# paste in to your web browser and add r at the end, and 
# you will have many solutions to choose from. R is not
# famous for being intuitive, but it is easy to get help!
