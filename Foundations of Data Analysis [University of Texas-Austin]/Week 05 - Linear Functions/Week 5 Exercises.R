# Week 5: Pre-lab, Lab, and Problem set
# Every four years, track and field athletes take the world stage at the Summer 
# Olympics.  Some of the most exciting events during each Olympics are those in 
# which athletes push the limits of their sport, breaking their own personal best 
# records, national records, or even world records.  We have compiled the world 
# record times for track events like the 100m dash and record distances for 
# field events like the shotput into a single dataset.  This dataset includes 
# information on the person who broke the record, his/her nationality, where the 
# record was broken, and the year it was broken.  Note that not all world records 
# are broken during the Olympics, with many occurring in regional or national competitions.

## Pre-lab
# Primary Research Question
# 
# How has the men's shotput world record changed over time?  
# What about the women's world record?

library(SDSFoundations)
WR <- WorldRecords

# 1) How many different types of events (e.g. "Mens 100m," "Womens shotput," etc.)
# are represented in the dataset?
length(unique(WR$Event))

# 2) In what year did Usain Bolt first break the world record for the men's 100m dash?
min(WR[which(WR$Athlete == 'Usain Bolt' & WR$Event == 'Mens 100m'),7])

# 3) Who was the first woman to break the women's 1 mile world record with a 
# time of less than 260 seconds?
WR[which(WR$Record<260 & WR$Event == 'Womens Mile'),]

#Subset the data
menshot <- WR[WR$Event=='Mens Shotput',]
womenshot <- WR[WR$Event=='Womens Shotput',] 

#Create scatterplots
plot(menshot$Year,menshot$Record,main='Mens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)
plot(womenshot$Year,womenshot$Record,main='Womens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)

#Run linear models
linFit(menshot$Year, menshot$Record)
linFit(womenshot$Year,womenshot$Record)

# 1)How many records are in the menshots data frame?
str(menshot)

# 2) How many records in the womenshot data frame?
str(womenshot)

## Lab
#Subset the data
mensmile <- WR[WR$Event=='Mens Mile',]
womensmile <- WR[WR$Event=='Womens Mile',] 

#Create scatterplots
plot(mensmile$Year,mensmile$Record,main='Mens Mile World Records',xlab='Year'
     ,ylab='World Record Time (s)',pch=16)
plot(womensmile$Year,womensmile$Record,main='Womens Mile World Records'
     ,xlab='Year',ylab='World Record Time (s)',pch=16)

#Run linear models
linFit(mensmile$Year, mensmile$Record)
linFit(womensmile$Year,womensmile$Record)

## Problem Set
## Question 1
#Subset the data
(menspole <- WR[WR$Event=='Mens Polevault' & WR$Year>=1970,])

# 1a. What is the standing world record height (in meters) for men's pole 
# vault? (Round to 2 decimal places.)
max(menspole$Record)

# 1b. In what year did the pole vault record first exceed 6 meters? 
# (Look at the data to find the year.)
min(menspole[which(menspole$Record>6),]$Year)

#Create scatterplots
plot(menspole$Year,menspole$Record,main='Mens Polevault World Records',xlab='Year'
     ,ylab='World Record Distance (m)',pch=16)

#Run linear models
linFit(menspole$Year, menspole$Record)

## Question 2
# The table below shows several points for the function C = f(h), where C is the 
# cost (in dollars) for a band of 4 members to play at a wedding, based on the 
# number of hours, h, they perform.
c <- c(140,280,420,560)
h <- c(0,2,4,6)

# 2b. Identify the y-intercept for this function.
linFit(h,c)

# 2d. If each member of the band earned $175 for the night and profits were split 
# evenly among them, how many hours did the band perform?
((175*4)-140)/70

## Question 3
# We have bivariate data on a group of college students: the total amount (in dollars) 
# spent on textbooks throughout their college career, and their GPA. The following 
# linear regression model was used to predict GPA from number of dollars (in hundreds) spent:
#     
#     Predicted GPA = 2.84 + .04*Dollars

# 3a. What is the predicted GPA of a student who spent a total of $970 on 
# textbooks in college? (Round to 2 decimal places.)
round(2.84+0.04*9.70,2)

# 3b. If a student spent $0 on textbooks in college and graduated with a GPA 
# of 3.71, what is her residual? (Round to 2 decimal places.)
round(3.71-(2.84+0.04*0),2)

# 3c. If a student spent $1,450 on textbooks and graduated with a GPA of 2.91, 
# what is his residual (Please indicate whether the residual is positive or 
# negative in your response, and round to 2 decimal places)?
round(2.91-(2.84+0.04*14.5),2)
