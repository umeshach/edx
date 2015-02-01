# Week 8: Pre-lab, Lab, and Problem Set


## Pre-lab
# Over 1,200 bull riders from around the world are members of Professional Bull Riders (PBR) and compete in the more than 300 PBR affiliated bull riding events per year. This data set includes information about the top 50 ranked bull riders for the 2013, according to the PBR standings reported in July of 2013. Rankings are based on a system which awards points for qualified rides at events throughout the season. More information available at: http://www.pbr.com/en/bfts/standings/riders.aspx.

# Primary Research Question

# The average American adult man weighs 190 pounds.  Do professional bull riders weigh the same?

library(SDSFoundations)
bull <- BullRiders

# Summarize the bull rider weights
mean(bull$Weight)
sd(bull$Weight)

# Visualize the weight distribution
hist(bull$Weight, main='Histogram of Bull Rider Weights',xlab='Weight (lbs)')

# Run the single sample t-test
t.test(bull$Weight, mu=190)

# 2a. Sample mean (in pounds)=
library(dplyr)
bull$Weight %>% mean(.) %>% round(.,2)

# 2b. Sample standard deviation (in pounds)=
bull$Weight %>% sd(.) %>% round(.,2)

# 3a. t-statistic (rounded to 1 decimal place)=
t.test(bull$Weight,mu=190)
round(t.test(bull$Weight,mu=190)$statistic,2)

# 3c/d. Lower/Upper bound estimate, in pounds (rounded to 1 decimal place)=
round(t.test(bull$Weight,mu=190)$conf.int,1)

## Lab
# Primary Research Question

# Do professional bull riders stay on their bulls at least 50% of the time? Test the hypothesis that the mean ride percentage is 50%.  (Hint: Two-sided test.)
hist(bull$RidePer)

# 1a. What was the average ride percentage? (round to 1 decimal place)
round(mean(bull$RidePer)*100,1)

# 1b. What was the standard deviation of ride percentage? (round to 1 decimal place)
round(sd(bull$RidePer)*100,1)

# 2a. What is the value of the t-statistic? (round absolute value to 2 decimal places)
t.test(bull$RidePer,mu=0.5)
round(t.test(bull$RidePer,mu=0.5)$statistic,2)

## Problem Set
# Question 1
# 
# How much money do professional bull riders earn by participating in an event?
bull$epe  <- bull$Earnings/bull$Events

# 1a. Have we met the assumptions for being able to calculate a 95% confidence interval to estimate the true mean earnings-per-event for a professional bull rider (using t)?
hist(bull$epe)

# 1b. Make a histogram of this log-transformed variable. Notice how the distribution shape has changed. Can we reliably calculate a 95% confidence interval for the mean of this transformed variable?
hist(log(bull$epe))
bull$epe.log <- log(bull$epe)

# 1c. What is the mean of the log-transformed earnings-per-event variable? (Round to 2 decimal places.)
round(mean(bull$epe.log),2)

# 1d. What are the lower and upper-bounds for a 95% confidence interval around this transformed mean? (Round each to 2 decimal places.)
t <- t.test(bull$epe.log,mu = mean(bull$epe.log))

# 1e. What are the lower and upper-bounds for a 95% confidence interval in dollars/event units. (Round each to whole numbers with no decimal places.)
#Lower
round(exp(8.53))
#upper
round(exp(9))

# Question 2
# 
# Students collected 8 random bags of a specific brand of potato chips and carefully weighed the contents of each bag, recording the following weights (in grams): 
#     
#     29.4      29.0      28.4      28.8      28.9      29.3      28.5      28.2 
# 
# The students want to test the claim that the mean weight of these bags is 28.5 grams.  They think it may be different. 
wt <- c(29.4,29,28.4,28.8,28.9,29.3,28.5,28.2)

# 2b. What are the sample mean and standard deviation? (Round each to 2 decimal places.)
round(mean(wt),2)
round(sd(wt),2)

# 2c. What is the test statistic for this hypothesis test? Remember: this is the t-statistic for the sample mean. (Round to 2 decimal places.)
t.test(wt,mu=28.5)

# 2d. What is t-critical for this test, assuming an alpha level of 0.05? (Round to 3 decimal places.)
round(qt(0.975,7),3)

# Question 3

# An industrial plant dumps its waste into a nearby river, but claims that it is not impacting the native species of frogs that live in the river.  The frogs are able to tolerate calcium concentrations up to 91 mg/L.  

# You measure the concentration of calcium in 25 random samples from the river.  Your measurements are approximately normally distributed, with a mean of 93.6 mg/L, with a standard deviation of 7.8 mg/L.  

# 3b. Calculate the test statistic. (Round to 2 decimal places.)
round((93.6-91)/(7.8/sqrt(25)),2)

# 3c. What is the t-critical value? (Round to 3 decimal places.)
round(qt(.95,24),3)

# Question 4
# 
# You are studying a population of peregrine falcons and want to estimate their average wingspan.  So you collect a random sample of 12 adult male birds and measure a mean wingspan of 42.6 cm, with a standard deviation of 5.3 cm. 
# 
# Assume that the distribution of measurements was approximately normal.
# 4a. What is t-critical for a 90% confidence interval? (Report as a positive value to 3 decimal places.)
round(qt(0.95,11),3)


# 4b. Calculate a 90% confidence interval for the mean wingspan for the population of male peregrine falcons. (Round to 2 decimal places.)
# Lower
round(42.6-round(qt(0.95,11),3)*(5.3/sqrt(12)),2)
# Upper
round(42.6+round(qt(0.95,11),3)*(5.3/sqrt(12)),2)
