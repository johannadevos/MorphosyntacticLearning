# Libraries
library(lme4)

# Clear workspace
rm(list=ls())

# Read in data
data <- read.csv("../data/Long_data.csv")
data$X <- NULL
str(data)

# Remove unaware participants
data <- data[data$learningtype!="unaware",]

