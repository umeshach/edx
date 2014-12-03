# Install (if necessary) and load SBSFoundations library
if ("SBSFoundations" %in% installed.packages()[,1]==F) {
    install.packages("SBSFoundations")
}

library("SBSFoundations")

# Set working directory
if (Sys.info()[4]=="SSAI-0102-HP") {
    setwd("~/edX/Foundations of Data Analysis [University of Texas-Austin]/Week 02 - Univariate Descriptive Statistics")
} else {
    print("... manually set your working directory ...")
}

# Download AnimalData.csv and read in data set
if (file.exists("./data/AnimalData.csv")==F) {
    url <- "https://courses.edx.org/c4x/UTAustinX/UT.7.01x/asset/AnimalData.csv"
    download.file(url,destfile="./data/AnimalData.csv")
}

animaldata <- read.csv("./data/AnimalData.csv")

# How many variables are in this dataset?
ncol(animaldata)

# How many of the first 10 animals in the dataset were adopted?
nrow(subset(animaldata[1:10,],Outcome.Type=="Adoption"))

# Was the first owner-surrendered animal in the dataset neutered?
subset(animaldata,Intake.Type=="Owner Surrender")[1,]$Neutered.Status=="Neutered"


#Find the number of animals that were adopted
table(animaldata$Outcome.Type)

#Pull out only adopted animals
adopted <- animaldata[animaldata$Outcome.Type=="Adoption",]

#Pull out just the days in shelter for the adopted animals
daystoadopt <- adopted$Days.Shelter

#Visualize and describe this variable
hist(daystoadopt)
fivenum(daystoadopt)
mean(daystoadopt)
sd(daystoadopt)
which(animaldata$Days.Shelter==max(daystoadopt))

# How many days was this animal in the shelter?
max(daystoadopt)

# What was the z-score for this particular animal? Round to the nearest ONE decimal places.
# note: z-score for adopted animals
scale(daystoadopt)[which(daystoadopt==max(daystoadopt))]
