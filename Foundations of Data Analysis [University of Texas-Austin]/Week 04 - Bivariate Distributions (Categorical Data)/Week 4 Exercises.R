# Week 4: Pre-lab, Lab, and Problem set
# Known as the ???Live Music Capital of the World,??? Austin, Texas is also home to 
# the longest-running music series in American television history, Austin City 
# Limits.  This dataset includes data on a sample of musicians that performed live 
# on the PBS television series Austin City Limits over the last 10 years.  Data on 
# each artist include measures of commercial popularity, such as the number of social 
# media followers on Twitter or Facebook, and their success in winning a Grammy Music Award. 
# 
# Primary Research Question
# 
# Do female artists play different kinds of music on Austin City Limits than male artists?

library(SDSFoundations)
acl <- AustinCityLimits

## Pre-lab
# 1) How many artists are in this dataset?
length(unique(acl$Artist))

# 2) How many of the first 10 artists in the dataset were Grammy winners?
table(head(acl$Grammy,10))

# 3) What genre was played by the first female artist in the dataset who was over 60 years of age?
head(acl[which(acl$Age>60),]$Genre,1)

# Create tables of marginal distributions
genre <- table(acl$Genre)
genre
gender <- table(acl$Gender)
gender

# Create contingency table 
twoway <- table (acl$Gender,acl$Genre)
twoway

# Visualize the counts
barplot(twoway, legend=T, beside=T)

# Calculate P(A): the probability of each genre being played
prop.table(genre)

# Calculate P(A|B): the probability of each genre being played, given the artist???s gender
prop.table(twoway,1)

## Lab
# 1) How many artists won a Grammy? and 2) How many artists did not win a Grammy?
table(acl$Grammy)

# Which genre had the greatest number of Grammy wins?
table(acl$Grammy,acl$Genre)

# 4) What is the probability that a randomly selected artist was a Grammy winner? 
# (Report as a proportion rounded to three decimal places)
round(prop.table(table(acl$Grammy)),3)

# What is the probability that a randomly selected artist from each of the 
# following genres won a Grammy? (Report as proportions rounded to three decimal places.)
round(prop.table(table(acl$Grammy,acl$Genre),2),3)

## Problem Set
# Question 1: You want to see if an artist's popularity on Facebook (whether or 
# not they have 100K+ likes) has anything to do with their age.

# 1a. How many artists in the dataset have 100K+ likes on Facebook?
table(acl$Facebook.100k)

# 1b. Which age group has the highest number of artists that have 100K+ likes 
# on Facebook? (Spell out your answer, i.e. twenties, thirties, forties, etc.)
table(acl$Facebook.100k,acl$Age.Group)

# 1c. For each age group, fill in the proportion of artists who have 100K+ likes 
# on Facebook. (Use the appropriate function in R to calculate these, and round 
#               to 3 decimal places (i.e. 0.123.)
round(prop.table(table(acl$Facebook.100k,acl$Age.Group),2),3)

# Question 2: A high school counselor wants to categorize students according to 
# two variables: their gender (male or female) and their grade level 
# (freshman, sophomore, junior or senior).
econ <- as.table(matrix(c(5,8,11,9,10,10,5,9,9,9,4,4,10,7,4,2,6,4,2,0),nrow=4,ncol=5))
row.names(econ) <- c('First','Second','Third','Fourth')
colnames(econ) <- c('A','B','C','D','F')

# 2b. What proportion of students in the class received a grade of A? (Round to 2 decimal places.)
round(sum(econ[,1])/sum(econ),2)

# 2c. What proportion of the students were upperclassmen (juniors and seniors)? (Round to 2 decimal places.)
round(sum(econ[3:4,])/sum(econ),2)

# 2d. What is the probability that a freshman received a failing grade of F? (Round to 2 decimal places.)
round(prop.table(econ,1),2)

# 2e. What is the probability that a randomly selected student from the class would 
# be a sophomore that received a grade of B? (Round to 2 decimal places.)
round(prop.table(econ),2)

# 2f. What proportion of juniors passed the course with a grade of D or better? (Round to 2 decimal places.)
round(sum(prop.table(econ,1)[3,1:4]),2)

# 2g. What is the probability that a randomly selected student from this class would be a senior? (Report to 2 decimal places.)
round(sum(econ[4,])/sum(econ),2)

# 2h. If a student received a grade of D in the class, what is the probability 
# that the student was a senior? (Round to 2 decimal places.)
round(prop.table(econ,2),2)

# Question 3
# Use the below probability statements to answer the following two questions. 
# Report answers as proportions.
# P(A) = 0.35
# P(A and B) = 0.15

# 3a. If A and B are independent, what is the value of P(A|B)? (Round to 2 decimal places.)
# P(A|B) = P(A)

# 3b. What is the probability of P(B|A)? (Round to 2 decimal places.)
# P(B|A) = P(A and B)/P(A)
round(0.15/0.35,2)

# Question 4
# A movie theater conducted a survey to determine the movie preferences of men 
# and women. They asked a total of 130 adults (50 women and 80 men) to choose their 
# favorite movie genre out of four choices: Action, Comedy, Horror, or Romance. 
# The results of their survey are shown below.

# 4b. What is the probability that a randomly chosen person from the survey 
# prefers Action films? (Report as proportion rounded to 2 decimal places.)
round((.12*50+.35*80)/130,2)
