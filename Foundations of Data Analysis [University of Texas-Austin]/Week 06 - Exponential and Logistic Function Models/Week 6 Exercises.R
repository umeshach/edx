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

## Lab
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

# 1. Create a variable that represents proportion of the population using the
# internet. (internet users divided by population).
# 2. Create a subset of the data that only contains data from 1990 onward.
# 3. Create a new variable that is "years since 1990". 
bd <- subset(world,Country %in% c('Denmark','Belarus'))
bd$prop.internet <- bd$internet.users / bd$population
bd_select <- subset(bd,year >= 1990)
bd_select$time <- bd_select$year - 1990

# 4. Create two new data frames --- one for each country of interest.
blr <- subset(bd_select,Country == 'Belarus')
dnk <- subset(bd_select,Country == 'Denmark')

# 5. Determine the best-fitting model (exponential or logistic) for internet usage
# in each country from 1990 onward.
expFit(blr$time,blr$prop.internet)
logisticFit(blr$time,blr$prop.internet)

expFit(dnk$time,dnk$prop.internet)
logisticFit(dnk$time,dnk$prop.internet)
