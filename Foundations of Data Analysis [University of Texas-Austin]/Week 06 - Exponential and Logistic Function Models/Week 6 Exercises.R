# Week 6: R Tutorials
library(SDSFoundations)
world <- WorldBankData
gbr <- world[world$Country.Code=='GBR',]
gbr2000 <- gbr[gbr$year>=2000 & gbr$year<2010,]
gbr2000$year_0 <- gbr2000$year-min(gbr2000$year)
expFit(gbr2000$year_0,gbr2000$motor.vehicles)
logisticFit(gbr2000$year_0,gbr2000$motor.vehicles)
tripleFit(gbr2000$year_0,gbr2000$motor.vehicles)

# Week 6: Pre-lab, Lab, and Problem set
# The World Bank is a data collection of information on all the world???s countries.
# Data is collected by country, and include items such as total population, CO2 
# emissions, and the number of mobile device subscriptions. We will examine some 
# of the trends in this dataset and interpret the parameters of the fitted models
# to best describe the change over time.

## Pre-lab
# Primary Research Question
# 
# What model best describes the first decade of internet usage (1990-1999)
# in the United States? Does this model hold through 2012?

# Clean up workspace, load library, load data
rm(list=ls())
library(SDSFoundations)
world <- WorldBankData

# 1a) What is the first ???Low Income??? country in the dataset?
world[world$IncomeGroup=="Low income",1][1]
subset(world,IncomeGroup == 'Low income',select = Country)[1,]

# 1b) What was the rural population of Aruba in 1970 (report without commas)?
subset(world,year==1970 & Country == 'Aruba', select = rural.population)

# 1c) When was the first year Australia had data on the number of mobile device 
# subscriptions? (Subscriptions more than 0)
subset(world,Country == 'Australia' & mobile.users > 0, select = year)[1,]

# Subset data for just the United States and name the new data frame "us"
us <- world[world$Country.Code == "USA",]

# Select the years from 1990 and name the new data frame "us_select"
us_select <- us[us$year >= 1990, ]

# Make the number of users more interpretable (into millions)
us_select$internet.mil <- us_select$internet.users / 1000000

# Create a new variable that is "years since 1990"
us_select$time <- us_select$year - 1990

# Select the first 10 years (from 1990 to 1999) and name the new data frame "us_select_10"
us_select_10 <- us_select[us_select$time < 10,]

# Use a function to fit an exponential and logistic model for 1990-1999
expFit(us_select_10$time, us_select_10$internet.mil)
logisticFit(us_select_10$time, us_select_10$internet.mil)

# Based on the prior model parameters, predict the number of internet users in 2006
e <- expFitPred(us_select_10$time, us_select_10$internet.mil, 16)
l <- logisticFitPred(us_select_10$time, us_select_10$internet.mil, 16)

# Show how many internet users the US actually had in 2006
us_select[us_select$time == 16, c("Country", "year", "internet.mil")]

# Calculate the residuals for each model
us_select$internet.mil[us_select$time == 16] - e
us_select$internet.mil[us_select$time == 16] - l

# Look at the model fits for all available data (1990 to 2012)
expFit(us_select$time, us_select$internet.mil)
logisticFit(us_select$time, us_select$internet.mil)

# Which model fits the best?
tripleFit(us_select$time, us_select$internet.mil)

# How many internet users would the US have had in 2012 if you had used the original exponential model?
expFitPred(us_select_10$time, us_select_10$internet.mil, 22)

## Lab 6: Worldwide Trends in Internet Usage
# The World Bank is a data collection of information on all the world???s countries.
# Data is collected by country, and include items such as total population, CO2
# emissions, and the number of mobile device subscriptions. We will examine some
# of the trends in this dataset and interpret the parameters of the fitted models
# to best describe the change over time.

# Primary Research Question
# 
# Denmark is a high-income country, and Belarus is a medium-income country of about
# the same size.  Find the best-fitting model for internet usage in each country
# since 1990.  Then answer the question:  Does income level have an impact on the
# speed with which a country adopts use of the internet? 

bd <- subset(world,Country %in% c('Denmark','Belarus'))
bd$prop.internet <- bd$internet.users / bd$population
bd_select <- subset(bd,year >= 1990)
bd_select$time <- bd_select$year - 1990

# 4. Create two new data frames --- one for each country of interest.
blr <- subset(bd_select,Country == 'Belarus')
dnk <- subset(bd_select,Country == 'Denmark')

# 5. Determine the best-fitting model (exponential or logistic) for internet usage
# in each country from 1990 onward.
expFit(dnk$time,dnk$prop.internet)
logisticFit(dnk$time,dnk$prop.internet)

expFit(blr$time,blr$prop.internet)
logisticFit(blr$time,blr$prop.internet)

# Using the logistic model equations from your analysis, calculate the YEAR that 10% of the population in each country would be using the internet.
# Denmark:
-(log10(0.89663-.1)-log10(308.8345*0.1))/log10(1.73124)
# Belarus:
-(log10(0.8987-.1)-log10(422.4322*0.1))/log10(1.31884)

# Using the logistic model equations from your analysis, calculate the YEAR that 80% of the population in each country would be using the internet.
# Denmark:
-(log10(0.89663-.8)-log10(308.8345*0.8))/log10(1.73124)
# Belarus:
-(log10(0.8987-.8)-log10(422.4322*0.8))/log10(1.31884)

## Problem Set
# Question 1
# How has mobile phone usage in Brazil changed since 1995?
bz <- subset(world,Country %in% c('Brazil'))
bz_select <- subset(bz,year >= 1995)
bz_select$time <- bz_select$year - min(bd_select$year)

# 1a. Find the number of mobile users in Brazil (in millions) in 2000, using R. (Round to 2 decimal places.)
round(subset(bz_select,year==2000,select=mobile.users)/1000000,2)

# 1b. In what year did Brazil first record more than 100 million mobile users?
subset(bz_select,mobile.users>=100000000,select=year)[1,]

# 1c. Generate a scatterplot and fit a linear, exponential and logistic model to the data. Which model best describes the increase in mobile users in Brazil since 1995?
tripleFit(bz_select$time,bz_select$mobile.users)


# 1d. What proportion of the variation in mobile users is explained by years since 1995 in the best-fitting model? (Round to 3 decimal places.)
round(logisticFit(bz_select$time,bz_select$mobile.users)$r_sq,3)

# 1e. Using the best-fitting model, predict the number of mobile users (in millions) in Brazil in 2025. (Round to zero decimal places.)
round(logisticFitPred(bz_select$time,bz_select$mobile.users,2025-1995)/1000000,0)

# Question 2
# 
# Records at the Center for Disease Control show that the total number of flu cases in Spring, 2009 looked like this:

# 2a. Looking at the raw data, what is the rate of change in flu cases from April 30 to May 1? (Report as a proportion rounded to 2 decimal places.)
round((367-257)/257,2)

# 2b. What is the growth rate for the flu, according to the exponential model? (Report as a proportion rounded to 2 decimal places.)

# 2c. Predict the number of cases of flu on Day 14 (when "Day" is equal to 14), using the exponential model. (Round to zero decimal places.)
round(76.64*1.46**14,0)

# 2d. Using the logistic model, predict the total number of flu cases on Day 14. (Round to zero decimal places.)
round(3273.31/(1+(43.59*1.57**-14)),0)

# 2e. The actual number of flu cases on Day 14 was 4,379. Find the residual of the exponential model prediction. (Round to zero decimal places.)
4379-round(76.64*1.46**14,0)

# 2f. What is the residual of the logistic model prediction for Day 14? (Round to zero decimal places.)
4379-round(3273.31/(1+(43.59*1.57**-14)),0)

# Question 3
# 3.  Yellowstone National Park began a project to restore its native wolf population in the mid 1990's. Below are the number of wolves soon after the start of the project:

# 3a. Researchers fit a linear model to the wolf data. Using this model, how many wolves were being added to the park each year? (Round to zero decimal places.)
x <- c(1,3)
y <- c(25,45)
linFit(x,y)

# 3c. Another researcher assumed that the wolves would experience exponential growth because there were no predators. He fit an exponential model to this data. What is the growth factor for his model? (Round to 2 decimal places.)
round(expFit(x,y)$b,2)

# 3d. What is the annual growth rate of these wolves each year, according to this model? (Report as a proportion rounded to 2 decimal places.)
round(expFit(x,y)$b-1,2)

# 3e. Assuming exponential growth, find the initial number of wolves when the project began. (Round to zero decimal places.)
round(expFitPred(x,y,0),0)

# 3f. By 2002, there were 147 wolves in Yellowstone Park. Which model was determined to fit the data better?
147-linFitPred(x,y,7)
147-expFitPred(x,y,7) #therefore, exponential

# 3g. Using the best-fitting model, how many years must pass before there are more than 325 wolves in Yellowstone? (Round to zero decimal places.)
round((log10(325)-log10(18.6339))/log10(1.34164),0)
