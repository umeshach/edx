# Week 9: Pre-lab, Lab, and Problem Set

## Lecture
#1c. What is the t-statistic?
(78-66)/sqrt((12.56^2/10+12.04^2/15))

# 1d. What is the t-critical value for this test, assuming df = nsmallest - 1 and ??=0.05?
qt(.95,9)

# 2c. What is the standard error for this test?
diff <- c(1,-1,-2,-2)
n <- 4
sd(diff)/sqrt(n)

# 2d. What is the value of the t-statistic and the appropriate conclusion for the test, assuming alpha of 0.05
t.test(diff,mu=0,df=3)

## Pre-lab
# Students at The University of Texas at Austin answered a set of questions for us at the beginning of the semester and then again at the end.  We'll use this data to compare different groups, and to explore what has (or has not) changed over time for these students. 

# Primary Research Questions

# 1.  Who is happier at the beginning of the semester:  lower-classmen or upper-classmen?
# 2.  Does student happiness change from the beginning of the semester to the end?
library(SDSFoundations)
post <- PostSurvey

# 1a. How many students are in the dataset?
nrow(post)

# 1b. What is the classification of the first male student? (Make sure your spelling matches the variable outcome as spelled in the dataframe.)
post[post$gender=="Male",4][1]

# 1c. Of the first 10 students in the dataset, what percentage live on campus? (Report without the "%" sign.)
prop.table(table(post$live_campus[1:10]))


# Lab Question 1

# Make a vector of happiness scores for each sample
underclass_happy <- post$happy[post$classification=='Freshman'|post$classification=='Sophomore']
upperclass_happy <- post$happy[post$classification=='Junior'|post$classification=='Senior']

# Check the normality assumption
hist(underclass_happy, xlab='Underclassman Happiness', main='Percent of Time Happy')
hist(upperclass_happy, xlab='Upperclassman Happiness', main='Percent of Time Happy')

# Run independent t-test
t.test(underclass_happy, upperclass_happy)

Lab Question 2

# Make a vector of difference scores
post$diff_happy <- post$happy - post$post_happy

# Check the normality assumption
hist(post$diff_happy, xlab= 'Difference in Happiness over the Semester', main = 'Happy-Post Happy')

# Run dependent t-test
t.test(post$happy, post$post_happy, paired=T)


# 1a. What percent of the time, on average, were underclassmen happy? (round to one decimal place)
round(mean(underclass_happy),1)

# 1b. What percent of the time, on average, were upperclassmen happy? (round to one decimal place)
round(mean(upperclass_happy),1)

# 1c. t-statistic=
t.test(underclass_happy, upperclass_happy)

## Lab
# Primary Research Questions
# 
# 1. Do students at UT spend more time on homework per week in college than they did in high school?
# 2. Do students in fraternities and sororities get less sleep on the weekends than other college students?
hs <- post$hw_hours_HS
col <- post$hw_hours_college
frat.sleep <- post$sleep_Sat[post$greek=="yes"]
nonfrat.sleep <- post$sleep_Sat[post$greek=="no"]

# 1a. On average, students spent how many hours more on homework each week in college than they did in high school? (round to 1 decimal)
round(mean(col-hs),1)

# 1b. What was the t-statistic for this test? (round to 2 decimal places)
t.test(col,hs,paired=T)

# 2a. On average, students who are not Greek sleep how many hours more than Greek students on Saturday nights? (report to 1 decimal place)
round(mean(nonfrat.sleep)-mean(frat.sleep),2)

# 2b. What is the t-statistic for this test? (report to 3 decimal places)
# 2c. How many degrees of freedom? (round to no decimal places)
# 2d. What was the p-value? (report to 3 decimal places)
t.test(frat.sleep,nonfrat.sleep,alt="less")

# 3. The Normality assumption (was/was not) unanswered met in each hypothesis test.
hist(hs)
hist(col)
hist(col-hs)
hist(frat.sleep)
hist(nonfrat.sleep)

## Problem Set
# Question 1
# 
# Is the increase in time spent studying from high school to college the same for nursing majors and biology majors?
post$hrs_increase <- post$hw_hours_college - post$hw_hours_HS
nursing <- post$hrs_increase[post$major=="Nursing"]
biology <- post$hrs_increase[post$major=="Biology"]

# 1b. Create a histogram to confirm the normality assumption for each sample. Has the normality assumption been met?
hist(nursing)
hist(biology)

# 1c. Run the appropriate t-test for this analysis. What is the t-statistic? (Report as a positive number rounded 2 decimal places.)
# 1d. How many degrees of freedom are there for this test? (Round to 2 decimal places.)
# 1e. What is the p-value for this test? (Round to 2 decimal places.)
t.test(nursing,biology)

# Question 2
# 
# A study was conducted to compare the resting pulse rates of college smokers and non-smokers.  The data for a randomly selected group is summarized in the table below. Pulse rates were normally distributed within each group.
#smokers
n1 <- 26;xbar1 <- 80;sd1 <- 5
#non-smokers
n2 <- 32;xbar2 <- 74;sd2 <- 6

# 2c. How many degrees of freedom should we use for this test if we are to estimate rather than use a calculator?
min(n1,n2)-1

# 2d. What is t-critical, assuming ??=0.05? (Round to 3 decimal places.) Use your answer to 2c. to help.
round(qt(.95,25),3)

# 2e. Calculate the standard error. (Round to 2 decimal places.)
(se <- round(sqrt(sd1^2/n1 + sd2^2/n2),2))

# 2f. Calculate the test statistic. (Round to 2 decimal places, and use rounded values from previous answers.)
round((xbar1-xbar2)/se,2)

# Question 3

# Some nerve cells have the ability to regenerate. Researchers think that these cells may generate creatine phosphate (CP) to stimulate new cell growth.

# To test this hypothesis, researchers cut the nerves emanating from the left side of the spinal cord in a sample of rhesus monkeys, while the nerves on the right side were kept intact.  They then compared the CP levels (mg/100g) in nerve cells on both sides. 
left <- c(
    16.3    
    ,4.8		
    ,10.7	
    ,14.0	
    ,15.7	
    ,9.9		
    ,29.3	
    ,20.4	
    ,15.7	
    ,7.6	
    ,16.2	
    ,14.7	
    ,15.0	
    ,8.4	
    ,23.3	
    ,17.7	
    )

right <- c(
    11.5
    ,3.5
    ,12.8
    ,7.9
    ,15.2
    ,9.8
    ,24.0
    ,14.9
    ,12.6
    ,8.2
    ,8.4
    ,11.0
    ,12.5
    ,9.2
    ,17.5
    ,11.1
    )
d <- left-right

# 3c. What is the t-critical value? (Round to 3 decimal places.)
round(qt(.95,15),3)

# 3d. How much of a difference in creatine phosphate was observed, on average, between the left and right nerve cells? (Report as a positive value rounded 1 decimal place).
mean(d) 

# 3e. What is the Standard Deviation of the difference scores? (Round to 2 decimal places.)
round(sd(d),2)

# 3f. What is the Standard Error for your t-test? (Round to 2 decimal places.)
round(sd(d)/sqrt(16),2)

# 3g. What is your test statistic? (Round to 2 decimal places.)
t.test(d,mu=0)
