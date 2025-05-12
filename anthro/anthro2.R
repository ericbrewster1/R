#  w2.2 f24
# import, clean up and save a .csv file

# Step 1: create an object to hold the data

x <- read.csv("dominicans_land.csv")

# NOTE: the above syntax works ONLY if the file in question 
# is sitting in the working directory (which can be set 
# and changed in both R and RStudio)

# you can also type the path directly inside the parentheses

# or use the file.choose() inside the parentheses and
# navigate to the file

# Step 2: look at the file by typing the object name or head(x)

head(x)

#  wealth = wealth of offspring
#  wealthf = father's wealth
#  male = sex of offspring
#  age = age of offspring
#  fid = anonymized father ID
#  these are all coded variables

# for steps 3-6 overwrite object x each time

# Step 3: are any fathers counted more than once?

sameFather <- duplicated(x$fid, incomparables=NA)
length(sameFather[sameFather==TRUE])
# Yes, there are fathers counted more than once

# Step 4: sort by ID to put same father in adjacent rows

x <- x[order(x$fid),]

# Step 5 remove the age variable

x <- subset(x, select = -age)

 # Step 6 remove all rows where both wealth values are NA

noWealth <- which(is.na(x$wealth) & is.na(x$wealthf))
x <- x[-noWealth,]

# Step 7 save script as Mylastname_w2.2_f24.R and submit
