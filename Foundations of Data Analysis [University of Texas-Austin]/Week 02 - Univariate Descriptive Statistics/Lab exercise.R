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


# How many adult dogs/cats are in the shelter?
table(animaldata$Animal.Type[animaldata$Age.Intake>0])

# What is the shape of the distribution of weight for adult dogs?
hist(animaldata$Weight[animaldata$Age.Intake>0 & animaldata$Animal.Type =="Dog"])

# What is the shape of the distribution of weight for adult cats?
hist(animaldata$Weight[animaldata$Age.Intake>0 & animaldata$Animal.Type =="Cat"])

# Which measure of center should be used to describe the average weight of the adult cats?
summary(animaldata$Weight[animaldata$Age.Intake>0 & animaldata$Animal.Type =="Cat"])

# What is the standard deviation for the weight of the adult cats? Round to two decimal places.
sd(animaldata$Weight[animaldata$Age.Intake>0 & animaldata$Animal.Type =="Cat"])

# What is the z-score of a 13 pound adult cat? Round to one decimal point.
catweight <- animaldata$Weight[animaldata$Age.Intake>0 & animaldata$Animal.Type =="Cat"]
(13-mean(catweight))/sd(catweight)

# What proportion of adult cats weigh more than 13 pounds, according to your data? Use the following code to answer this question: 1-pnorm(zcat). Replace zcat with your z-score for the cat. Round to three decimal places.
1-pnorm((13-mean(catweight))/sd(catweight))

# What quartile would contain a 13-pound adult dog?
summary(animaldata$Weight[animaldata$Age.Intake>0 & animaldata$Animal.Type =="Dog"])


summary(catweight)
