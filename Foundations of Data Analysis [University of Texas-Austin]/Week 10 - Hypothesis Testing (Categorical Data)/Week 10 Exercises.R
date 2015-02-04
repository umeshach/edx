# Week 10: Lecture, Pre-lab, Lab, and Problem Set

## Lecture
# 2. A major snack food company claims that its chips are "America's favorite." A statistics class tests this claim by asking a sample of 90 random students on campus to select their favorite chip from the company's (Brand A) and two other brands (Brand B and Brand C). Below are the results of how many students selected each brand in their taste test.        
snacks <- c(38,28,24)

# Chi-square statistic: (Rounded to 2 decimal places.)
chisq.test(snacks)

# Chi-square critical value: (Rounded to 2 decimal places.)
qchisq(.95,2)

# 1. Jurors are selected from the list of registered voters, so the ages for jurors should have the same distribution as the ages of voters. A law professor obtains voter registration records and finds that 20% of registered voters are 18-29, 45% are 30-49, and 35% are age 50 or older. The professor then monitors jury composition over a month-long period and finds the following distribution of jurors:  
a <- c(12,36,32)
e <- c(.2,.45,.35)

# 1d. Find the expected values for each category, assuming the age distributions of jurors and voters are the same.
e*sum(a)

# 1e. Find the chi-square statistic using the following formula:
sum((a-(e*sum(a)))^2/(e*sum(a)))
chisq.test(a,p=e)

# 1g. What is the critical Chi-square value for this hypothesis test? (Rounded to 2 decimal places)
round(qchisq(.95,2),2)

