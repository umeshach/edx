# Known as the ???Live Music Capital of the World,??? Austin, Texas is also home to the longest-running music series in American television history, Austin City Limits.  This dataset includes data on a sample of musicians that performed live on the PBS television series Austin City Limits over the last 10 years.  Data on each artist include measures of commercial popularity, such as the number of social media followers on Twitter or Facebook, and their success in winning a Grammy Music Award.

library(SDSFoundations)
acl <- AustinCityLimits

## Pre-lab
# 1. Are there an equal number of male and female performers on Austin City Limits?
# 2. Are male performers just as likely to have had a Top 10 hit as female performers?
head(acl)
acl[acl$Artist == "Allen Toussaint",]

    
# Question 1 (Goodness of Fit)
# Create a table of counts for Gender
gender_tab <-table(acl$Gender)
gender_tab

# Create vector of expected proportions
ExpGender <- c(.50, .50)

# Check expected counts assumption
chisq.test(gender_tab, p=ExpGender)$expected

# Run goodness of fit
chisq.test(gender_tab, p=ExpGender)


# Question 2 (Test of Independence)
# Create two-way table
gender_top10 <-table(acl$Gender, acl$BB.wk.top10)
gender_top10

# Generate expected counts
chisq.test(gender_top10, correct=FALSE)$expected

# Run test of independence
chisq.test(gender_top10, correct=FALSE)

## Lab
# Primary Research Questions

# 1. Are each of the four musical genres equally represented on Austin City Limits?   
# 2. Are some genres more likely to draw a large (100K+) Twitter following than others?
genre <- table(acl$Genre)
 
# 1a. What was the expected count of artists for each genre?
chisq.test(genre)$expected
# 1b. What was the Chi-square statistic? (report to 2 decimal places)
chisq.test(genre)

# Test of Independence
# 2a. Using the data from your two-way table, compute the proportion of artists in each genre with 100K+ Twitter followers. (Round to 3 decimal places).
genre.twitter <- table(acl$Genre,acl$Twitter.100k)
genre.twitter
round(prop.table(genre.twitter,1),3)

# 2b. What was the Chi-square statistic? (report to 2 decimal places)
chisq.test(genre.twitter)$expected
chisq.test(genre.twitter,correct=F)

## Problem SEt
# Question 1
# 
# You want to know if the proportion of female performers on Austin City Limits Live has changed in the past two years. 
# You'll need to use the following code to help:
acl$Recent[acl$Year < 2012] <- 0 
acl$Recent[acl$Year >= 2012] <- 1
(gender.recent <- table(acl$Gender,acl$Recent))

# 1c. Report expected counts for the following performer groups.
chisq.test(gender.recent)$expected

# 1d. What is the Chi Square statistic? (Round to 2 decimal places.)
chisq.test(gender.recent,correct=F)

# Question 2
# 
# When crossing white and yellow summer squash, a genetic model predicts that 75% of resulting offspring will be white, 15% will be yellow and 10% will be green. 
# 
# Below are the results from an experiment run on a random sample of 205 squash offspring.
squash <- c(152,39,14)
squash.exp <- c(.75,.15,.1)

# 2b. What is the expected count of white offspring? (Round to 2 decimal places.
sum(squash)*squash.exp

# 2f. What are the degrees of freedom and the critical value for this test, assuming ?? = 0.05?
qchisq(.95,2)

# 2g. What is the Chi Square statistic for this test? (Round to 2 decimal places.)
chisq.test(squash,p=squash.exp)

# Question 3
# 
# Approximately 13% of the world's population is left-handed, but is this proportion the same across men and women?
# 
# To answer this question, you decide to collect data from a random sample of adults from your neighborhood, with the following results:
gender <- c("M","M","F","M","F","F","F","M","F","F","M","F","M","M","F","M","M","F","F","M","F")
hand <- c("L","R","R","R","R","L","L","R","R","R","L","R","R","R","R","R","R","R","L","R","R")

# 3b. What would be the degrees of freedom and the critical value for this analysis, assuming ?? = 0.05?
qchisq(.95,1)

# 3c. What are the expected counts for Males? (Round to 2 decimal places.)
(gender.hand <- table(gender,hand))
chisq.test(gender.hand)$expected

# Question 4
# 
# A telephone survey asked a random sample of Indiana voters about their home internet usage, as well as what type of community (rural, suburban or urban) they lived in. 
# 
# Of the 123 survey respondents, 28 were from rural areas, 42 were from suburban areas, and 53 were from urban areas.  Thirteen rural respondents, 35 suburban respondents, and 50 urban respondents said they had access to internet at home. 
(area <- c(rep("rural",28),rep("suburban",42),rep("urban",53)))
(internet <- c(c(rep(1,13),rep(0,15)),c(rep(1,35),rep(0,7)),c(rep(1,50),rep(0,3))))

(area.int <- table(area,internet))

# 4b. What proportion of respondents had internet access at home? (Round to 1 decimal place.)
sum(internet)/sum(area.int)

# 4c. What proportion of respondents did NOT have internet access at home? (Round to 1 decimal place.)
1-sum(internet)/sum(area.int)

# 4d. How many rural residents would we expect to have home internet? (Round to 2 decimal places.)
# 4e. How many urban residents would we expect NOT to have home internet? (Round to 2 decimal places.)
round(chisq.test(area.int)$expected,2)

# 4f. Does this data provide sufficient evidence that internet access at home depends on what type of community the Indiana voters live in?
chisq.test(area.int,correct=F)
