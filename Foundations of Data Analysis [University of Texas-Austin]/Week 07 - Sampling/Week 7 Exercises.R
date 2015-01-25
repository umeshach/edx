# Week 7: Pre-lab, Lab, and Problem Set
# In this lab, we will examine how sample data can be used to discover the truth about a population.  Our population data consists of data we collected from our statistics students here at The University of Texas at Austin.   They told us several things about themselves, including how happy they are and the amount of time they study.  We'll run a few simulations on this data to see if we can replicate what the Central Limit Theorem tells us about sampling. We are pretending that we don't know the "true" population parameters, but in fact we do!

# Primary Research Question
# How many letters long is the typical UT student???s name?  How does our estimate change as we increase the size of our sample?

library(SDSFoundations)
survey <- StudentSurvey

## Pre-lab
# 1a) How many students are in this dataset?
nrow(survey)

# 1b) How many of the first 10 students in the dataset had names longer than 5 letters?
sum(survey$name_letters[1:10]>5)

# 1c) How long is the name of the first student in the dataset who is happy less than 40% of the time?
subset(survey,happy<40)[1,]$name_letters

# Calculate the population parameters
hist(survey$name_letters)
fivenum(survey$name_letters)
mean(survey$name_letters)
sd(survey$name_letters)

# Draw 1,000 samples of n=5 and find the mean of each sample.
xbar5 <-rep(NA, 1000)
for (i in 1:1000) {
    x <- sample(survey$name_letters, size =5)
    xbar5[i] <- mean(x)
}

# Graph the histogram of 1,000 sample means.
hist(xbar5,xlim=c(2,10))
summary(xbar5)
fivenum(xbar5)

# Calculate the mean and sd of the sampling distribution.
mean(xbar5)
sd(xbar5)

# Compare to the std dev predicted by the CTL.
sd(survey$name_letters)/sqrt(5)


# Function to do sampling and compare
samplingcompare <- function(pop,samples=30,resamples=1000) {
    samplemeans <- rep(NA,resamples)
    for (i in 1:resamples) {
        x <- sample(pop,size=samples)
        samplemeans[i] <- mean(x)
    }
    sampling  <- list()
    sampling$samplemeans <- samplemeans
    sampling$meanofsamples <- mean(samplemeans)
    sampling$sdofsamples <- sd(samplemeans)
    sampling$standarderror <- sd(pop)/sqrt(samples)
#     hist(samplemeans,xlim=c(2,10))
    return(sampling)
}

#Repeat for samples of size n=15
samplingcompare(survey$name_letters,15)

#Repeat for samples of size n=25
samplingcompare(survey$name_letters,25)

# 1a) What is the average name length, in number of letters, for all of the students in the population? (Round to 2 decimal places.)
round(mean(survey$name_letters),2)

# 1b) By how many letters, on average, do names vary from the mean? (Round to 2 decimal places.)
round(sd(survey$name_letters),2)


# 4a) What is the mean of the sampling distribution (for n=5, 15, or 25)? (Round to 2 decimal places)
round(mean(c(samplingcompare(survey$name_letters,5)$meanofsamples,samplingcompare(survey$name_letters,15)$meanofsamples,samplingcompare(survey$name_letters,25)$meanofsamples)),2)
round(samplingcompare(survey$name_letters,5)$meanofsamples,2)
round(samplingcompare(survey$name_letters,15)$meanofsamples,2)
round(samplingcompare(survey$name_letters,25)$meanofsamples,2)

# 4b) What is the standard error of the sampling distribution for n=5?
samplingcompare(survey$name_letters,5)$standarderror
# 4c) What is the standard error of the sampling distribution for n=15?
samplingcompare(survey$name_letters,15)$standarderror
# 4d) What is the standard error of the sampling distribution for n=25?
samplingcompare(survey$name_letters,25)$standarderror

## Lab
# Primary Research Question
# What percentage of the time are college students happy?  How does our estimate of the true mean change as sample size increases?
happy <- survey$happy
hist(happy)
summary(happy)
fivenum(happy)

# Compare the sample statistics:  
#     3. Draw 1,000 samples of size n=5 from the population data.  Calculate the mean of each sample. 
# 4. Graph these 1,000 sample means in a histogram and examine the shape.
# 5. Calculate the mean and standard deviation of the sampling distribution.
# 6. Repeat this process for samples of size n=15 and n=25.
# 7. Compare the results you get to the predictions of the Central Limit Theorem.
# 5 samples
n5 <- samplingcompare(happy,5)
hist(n5$samplemeans)
n5$meanofsamples
n5$sdofsamples
n5$standarderror
# 15 samples    
n15 <- samplingcompare(happy,15)
hist(n15$samplemeans)
n15$meanofsamples
n15$sdofsamples
n15$standarderror
# 25 samples
n25 <- samplingcompare(happy,25)
hist(n25$samplemeans)
n25$meanofsamples
n25$sdofsamples
n25$standarderror
# population
mean(happy)
sd(happy)

## Problem Set
# Question 1
# 
# On a scale of 1 to 10, how much do UT Austin students like Austin?
# 
# 1. What are the true mean and standard deviation for our population of UT Austin students?
# 2. What should the sampling distribution of the mean look like, as predicted by the Central Limit Theorem?
# 3. How do our simulated values compare to these predicted values?
austin <- survey$austin
hist(austin)
summary(austin)

# 1b. What is the population mean for the "austin" variable? (Round to 2 decimal places.)
round(mean(austin),2)

# 1c. What is the population standard deviation for the "austin" variable? (Report to 2 decimal places.)
round(sd(austin),2)

# 1d. Use the Central Limit Theorem to predict the mean and standard deviation of the sampling distribution of means for samples of size n=10 drawn from this population: 
# What is the expected mean? (Round to 2 decimal places.)
round(mean(austin),2)
# What is the expected standard deviation? (Round to 2 decimal places.)
round(sd(austin)/sqrt(10),2)

# Question 2
# 
# A population of sunflower plants is described as having a monthly growth rate that follows a normal distribution with ?? = 3.08 in and ?? = 0.40 in.
# 
# Use this information to answer the following questions.

# 2a. What is the probability that a randomly chosen sunflower plant grows more than 3.2 inches in a month? (Round to 3 decimal places.)
round(1-pnorm((3.2-3.08)/0.4),3)

# 2b. A middle-school science class grew 25 of these sunflowers. How many inches would they expect these flowers to have grown, on average, one month later? (Round to 2 decimal places.)
# Expected to be equal to pop'n mean

# 2c. The middle school science teacher replicates her study with 25 new sunflowers every year. How much variability should she expect in the average monthly growth of these samples? (Round to 2 decimal places.)
# Standard error
round(0.4/sqrt(25),2)

# 2d. The science teacher notices that the average monthly growth of her 25 sunflowers has never exceeded 3.2 inches. What should she conclude?
round((1-pnorm((3.2-3.08)/(0.4/sqrt(25)))),3)
# Only 6.7% probability that average growth rate of her 25-samples will exceed 3.2inches, therefore, we can say that her data is probably fine.

# 2e. What is the probability that her next sample of 25 sunflowers will grow an average of more than 2.9, but less than 3.2 inches, in a month? (Report as a proportion rounded to 3 decimal places.)
round(pnorm((3.2-3.08)/(0.4/sqrt(25)))-pnorm((2.9-3.08)/(0.4/sqrt(25))),3)

# Question 3
# 
# A very large company has its headquarters in a 15-story downtown office building. The morning commute time for employees of this company is normally distributed with a mean of 28 minutes and a standard deviation of 11 minutes.
# 
# The company in the building next door samples 23 of its employees and finds that their mean commute time is 35.1 minutes. Is there evidence that their commute time is longer than the other company's, or is this just random sampling error?
# 
# Use the Central Limit Theorem to determine if this sample mean is likely to be observed, assuming commute time is the same for both companies.
mu <-28 
sd <- 11
xbar <- 35.1
n <- 23

# 3a. What is the expected mean of the sampling distribution for samples of size n=23? (Report as a whole number)
# Equal to the pop'n

# 3b. What is the standard error of the sampling distribution for samples of n=23? (Round to 2 decimal places.)
round(sd/sqrt(n),2)

# 3c. What is the z-score for the neighboring company's sample mean? (Round to 1 decimal place.)
round((xbar-mu)/(sd/sqrt(n)),1)

# 3d. What is the probability of observing a sample mean this high (or higher), if the employees really do commute the same amount of time?
1-pnorm((xbar-mu)/(sd/sqrt(n)))

# Question 4
# 
# Dixie Queen uses an automatic ice cream dispenser to fill pint-sized containers of ice cream. The company that makes the dispenser says the volume it dispenses into each container follows a normal distribution with ??= 1.5 ml.
# 
# The Dixie Queen manager randomly selected 15 ice cream pints and found that the average volume was 471.46 ml. She wants to know if her machine is performing as expected.

sd <- 1.5
xbar <- 471.46
n <- 15

# 4a. What is the expected variability in sample means of size n=15? (Find the standard error and round to 3 decimal places.)
round(sd/sqrt(n),3)

# 4b. What is the margin of error, assuming 95% confidence? (Round to 3 decimal places and use this value in the following calculations.)
round(qnorm(0.975)*(sd/sqrt(n)),3)

# 4c. Find the 95% confidence interval for the mean volume for this sample of 15 randomly selected ice cream pint containers.
# Lower bound
round(xbar-qnorm(0.975)*(sd/sqrt(n)),1)
# Upper bound
round(xbar+qnorm(0.975)*(sd/sqrt(n)),1)

# 4d. A pint is equivalent to 473.20 ml. Do you think the dispenser is working as reported?
pnorm((xbar-473.2)/(sd/sqrt(n)))


