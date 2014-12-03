# Lab 1: Cycling in Austin
# Question: How many of the cyclists were students, how often did they ride, and what was the average distance they rode?

# set working directory
setwd("~/edX/Foundations of Data Analysis [University of Texas-Austin]/Week 01 - Introduction to Data")

# download file
if (!file.exists("./data/BikeData.csv")) {
    download.file("https://courses.edx.org/c4x/UTAustinX/UT.7.01x/asset/BikeData.csv",destfile="./data/BikeData.csv",mode="wb")    
}

# read in data
BikeData <- read.csv("./data/BikeData.csv")

BikeData$age[7]
length(BikeData$cyc_freq[1:10][BikeData$cyc_freq[1:10]=="Daily"])
BikeData[BikeData$gender=="F",9][1]

#show number of students
table(BikeData$student)

#Pull out student data into a new data frame
student <-BikeData[BikeData$student==1,]

#Find how often the students ride
table(student$cyc_freq)

#Create vector for the variable distance
distance <-student$distance
distance

#Find average distance ridden
mean(distance)
