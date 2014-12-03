# Set working directory
if (Sys.info()[4]=="SSAI-0102-HP") {
    setwd("~/edX/Foundations of Data Analysis [University of Texas-Austin]/Week 02 - Univariate Descriptive Statistics")
} else {
    print("... manually set your working directory ...")
}


# Install (if necessary) and load SBSFoundations library
if (file.exists("SDSFoundations_1.1.zip")==F) {
    url <- "https://preview.edx.org/c4x/UTAustinX/UT.7.01x/asset/SDSFoundations_1.1.zip"
    download.file(url,destfile="SDSFoundations_1.1.zip")
}

if ("SDSFoundations" %in% installed.packages()[,1]==F) {
    install.packages("SDSFoundations_1.1.zip",repos=NULL)
}

library("SDSFoundations")

# Download AnimalData.csv and read in data set
if (file.exists("./data/AnimalData.csv")==F) {
    url <- "https://courses.edx.org/c4x/UTAustinX/UT.7.01x/asset/AnimalData.csv"
    download.file(url,destfile="./data/AnimalData.csv")
}

animaldata <- read.csv("./data/AnimalData.csv")


# What was the most common way that dogs arrived in the shelter? (as defined by the ???Intake.Type??? variable)
table(animaldata$Intake.Type)

# What proportion of dogs were brought to the shelter as an owner surrender? (Round to 3 decimal places.)
table(animaldata$Intake.Type[which(animaldata$Animal.Type=="Dog")])[2]/nrow(subset(animaldata,Animal.Type=="Dog"))

# Of the dogs that were brought to the shelter as an owner surrender, how many were returned to their owner?
nrow(subset(animaldata,Animal.Type=="Dog" & Intake.Type=="Owner Surrender" & Outcome.Type =="Return to Owner"))

# What was the mean number of days that these dogs spent at the shelter before being returned to their owner? (Round to 1 decimal place.)
mean(subset(animaldata,Animal.Type=="Dog" & Intake.Type=="Owner Surrender" & Outcome.Type =="Return to Owner")$Days.Shelter)


# Suppose that hours of sleep per night for single adults between 30 and 40 years of age are normally distributed with a mean of 6.7 hours and a standard deviation of 1.1 hours.  

# 4a. If an adult has a z-score of -1.5, how many hours of sleep does this person get per night? (Report to 2 decimal places.)
6.7-(1.1*1.5)

# What proportion of adults sleep longer than 4.5 hours per night? (Report to 3 decimal places.)
1-pnorm((4.5-6.7)/1.1)

# 4c. What proportion of adults sleep between 5.38 and 8.79 hours of sleep? (Report to 3 decimal places.)
pnorm((8.79-6.7)/1.1)-pnorm((5.38-6.7)/1.1)
