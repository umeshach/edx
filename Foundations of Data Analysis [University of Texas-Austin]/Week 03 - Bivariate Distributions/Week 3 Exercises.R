# Week 3: Pre-lab, Lab, and Problem set
# Primary Research Question
# 
# Is there a linear relationship between how often a rider places in the Top 10 
# and how often he stays on his bull for a full 8 seconds? 

library(SDSFoudnations)

# load dataset
br <- BullRiders

# number of obs
nrow(br)

# How many of the first 10 riders in the dataset have been pro for more than 10 years?
length(which(br[1:10,6]>10))

# How many rides were completed by the rider with the fewest buck-outs?
br[which(br$BuckOuts==min(br$BuckOuts)),10]

# Visualize and describe the first variable of interest 
hist(br$RidePer)
fivenum(br$RidePer)
mean(br$RidePer)
sd(br$RidePer)

# Scatterplot with line of best fit
plot(br$RidePer, br$Top10)
abline(lm(br$Top10~br$RidePer), col="red")

# Calculate the correlation coefficient
cor(br$RidePer,br$Top10)

# Create a correlation matrix  
cor(br[,c(16,13)])

# What percent of the time, on average, does a bullrider succeed in staying 
# on his bull? (Round to the nearest whole number.)
mean(br$RidePer)

# How many riders stayed on their bull more than 60% of the time?
length(which(br$RidePer>0.6))

# What is the shape of the Earnings distribution?
hist(br$Earnings)

# What was the average amount earned by a bull rider? (Choose the appropriate 
# measure of center; report without a $ sign and round to the nearest whole number.)
round(median(br$Earnings))

# What was the highest amount earned by a bullrider? (Report without a $ sign and 
# round to the nearest whole number.)
round(max(br$Earnings))

# Make a Scatterplot of Earnings and Ride Percentage
plot(br$RidePer,br$Earnings)

# What is the correlation of Earnings with Ride Percentage? (report to three decimal places)
round(cor(br$RidePer,br$Earnings),digits=3)

# Create a Scatterplot of Earnings and Cup Points
plot(br$Earnings,br$CupPoints)
plot(br$CupPoints,br$Earnings)

# What is the correlation of Earnings with Cup Points? (report to three decimal places)
round(cor(br$Earnings,br$CupPoints),3)

# After removing the outlier, what was the new correlation of Earnings and Ride Percentage? (Round to three decimals)
brnooutlier <- br[-which(br$Earnings == max(br$Earnings)),]
round(cor(brnooutlier$Earnings,brnooutlier$RidePer),3)

# After removing the outlier, what was the new correlation of Earnings and Cup Points? (Round to three decimals)
round(cor(brnooutlier$Earnings,brnooutlier$CupPoints),3)


## Problem Set
# Question 1
# During a professional bull-riding event, riders usually attempt to ride a bull 
# three or more times.  This means that they can record a "ride" (successfully 
# staying on the bull) multiple times in the same event.

# Create a new variable for the average number of rides per event for each bull rider in the dataset:
br$RidesPerEvent <- br$Rides/br$Events
hist(br$RidesPerEvent)

# 1a. What is the minimum value? (Round to 2 decimal places.)
round(min(br$RidesPerEvent),2)

# 1b. What is the median?
median(br$RidesPerEvent)

# 1c. What is the maximum value? (Round to 2 decimal places.)
round(max(br$RidesPerEvent),2)

# 1d. Create a scatterplot of "rides per event" and yearly ranking (defined by 
# the "Place" variable) and add a line of best fit. Which of the following best describes the relationship between these two variables?
plot(br$RidesPerEvent,br$Place)

# 1e. What is the correlation coefficient for rides per event and yearly ranking? (Report to 3 decimal places)
round(cor(br$RidesPerEvent,br$Place),3)

# 1f. Suppose that college GPA and graduate school GPA have a correlation 
# coefficient of 0.75. Based on this, what proportion of variation in graduate 
# school GPA is left unexplained after taking college GPA into account? 
# (Report to 4 decimal places)?
1-0.75**2

# Using the dataset below, find the correlation coefficient between time spent studying and exam grade.
time <- c(30,45,180,95,130,140,30,80,60,110,0,80)
grade <- c(74,68,87,90,94,84,92,88,82,93,65,90)

# 2a. What is the correlation coefficient based on the data? (Round to 3 decimal places.)
round(cor(time,grade),3)

# 2b. Approximately what percentage of the variation in exam scores can be 
# explained by the amount of time that each student studied? (Report to whole number without a % sign.)
round((cor(time,grade)**2)*100,0)

# 2c. Create a scatterplot of the data (exam grades and time spent studying). 
# What is the value of the outlier (the student that got a high grade but didn't study very long)?
plot(time,grade)
time[which(time<50)];grade[which(time<50)]

# 2d. When the outlier is removed, what is the new value of r? (Round to 3 decimal places.)
round(cor(time[-7],grade[-7]),3)
