# Copyright Eva Koch (2018)

# This script was created in July 2018.
# The script was written to answer our secondary research questions. These concern a) the control items, b) test performance at
# T3 (explicit posttest), and c) test performance of the unaware [-TS,-LP] group (n=6).
# We used 'the new statistics' to answer these questions. Our basic source was Jenifer Larson-Hall's (2016) manual ("A guide
# to doing statistics - Second Edition"). This method involves a focus on effect sizes and confidence intervals, while taking the focus away
# from the dichotomous thinking related to NHST (i.e., p-values).

# Clear workspace
rm(list=ls())

# Set working directory to source file location
set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path # get the path of your current open file
  setwd(dirname(current_path )) # set the working directory to the source file location
  print( getwd() ) # display the directory
}
set_wd()

# Read in data
data <- read.csv("../data/Data_participants_wide.csv")

# Load packages
library(fBasics) # for descriptive statistics
library(boot) # for bootstrapped confidence intervals
library(ggplot2) # for plots
library(plotrix) # for SE

# The steps to be followed:
  # Determine which conrasts I need to investigate (based on research questions)
  # Calculate the means and their SDs for the contrasts
  # Check normality (per specific factor level combination; this will tell us whether the mean is actually a good point estimate of the data)
  # Check homogeneity of variances (this is important if we want to use the pooled SD to calculate Cohen's d)
  # Calculate the mean difference and its SE
  # Calculate the CI of the mean difference using bootstrappign (10.000)
  # Calculate effect size (Cohen's d)
  # Calculate the CI of the effect size (use Cumming's ESCI file)
  # Put all the results in a single Excel table

attach(data)
# Calculate new variable: critical items (across test moment, input)
data$crit <- (crit_input_T1 + crit_noinput_T1 + crit_input_T2 + crit_noinput_T2 + crit_input_T3 + crit_noinput_T3)/6

# Calculate new variable: control items (across test moment, input)
data$contr <- (contr_input_T1 + contr_noinput_T1 + contr_input_T2 + contr_noinput_T2 + contr_input_T3 + contr_noinput_T3)/6

# Calculate new variable: critical items at T1 (across input)
data$crit_T1 <- (crit_input_T1 + crit_noinput_T1)/2

# Calculate new variable: control items at T1 (across input)
data$contr_T1 <- (contr_input_T1 + contr_noinput_T1)/2

# Calculate new variable: critical items at T2 (across input)
data$crit_T2 <- (crit_input_T2 + crit_noinput_T2)/2

# Calculate new variable: control items at T2 (across input)
data$contr_T2 <- (contr_input_T2 + contr_noinput_T2)/2

# Calculate new variable: critical items at T3 (across input)
data$crit_T3 <- (crit_input_T3 + crit_noinput_T3)/2

# Calculate new variable: control items at T3 (across input)
data$contr_T3 <- (contr_input_T3 + contr_noinput_T3)/2

# Calculate new variable: critical items with input (across test moment)
data$crit_input <- (crit_input_T1 + crit_input_T2 + crit_input_T3)/3

# Calculate new variable: control items with input (across test moment)
data$contr_input <- (contr_input_T1 + contr_input_T2 + contr_input_T3)/3

# Calculate new variable: critical items without input (across test moment)
data$crit_noinput <- (crit_noinput_T1 + crit_noinput_T2 + crit_noinput_T3)/3

# Calculate new variable: control items without input (across test moment)
data$contr_noinput <- (contr_noinput_T1 + contr_noinput_T2 + contr_noinput_T3)/3

# Calculate new variable: critical items across T1 and T2 (across input)
data$crit_T1_T2 <- (crit_input_T1 + crit_noinput_T1 + crit_input_T2 + crit_noinput_T2)/4

# Calculate new variable: control items across T1 and T2 (across input)
data$contr_T1_T2 <- (contr_input_T1 + contr_noinput_T1 + contr_input_T2 + contr_noinput_T2)/4
detach(data)


# Prepare dataset for the a) and b) analyses (exclude unaware participants)
data.2groups <- data[data$learningtype!="unaware",] # remove unaware participants from dataset
data.2groups$learningtype <- factor(data.2groups$learningtype) # drop unused factor level


# Create separate datasets for the two groups
data.2groups.exp <- data.2groups[data.2groups$learningtype!="incidental",] # remove incidental participants from dataset
data.2groups.exp$learningtype <- factor(data.2groups.exp$learningtype) # drop unused factor levels

data.2groups.inc <- data.2groups[data.2groups$learningtype!="explicit",] # remove incidental participants from dataset
data.2groups.inc$learningtype <- factor(data.2groups.exp$learningtype) # drop unused factor levels


# Prepare dataset for c) (only unaware participants)
data.unaware <- data[data$learningtype=="unaware",]
data.unaware$learningtype <- factor(data.unaware$learningtype)



# Calculation of the CI of the effect size:
# Use Cumming's (2012) ESCI file; the free software can be found here:
# https://www.latrobe.edu.au/psychology/research/research-areas/cognitive-and-developmental-psychology/esci/understanding-the-new-statistics)
# Open the ESCI chapters 5-6 Excel file; go to the "Data two" tab.
# Enter your data. Scroll to the right in the tab and activate the "Display d and CI" case.
# This procedure is also explained in Larson-Hall (2016), p.151.
# Results to obtain through the ESCI file: d, d CIs, d MOE.


# Write csv files to facilitate copy-pasting data into ESCI (after running the R script)
write.csv(data.2groups, file="../results/data-2groups.csv")
write.csv(data.2groups.exp, file="../results/data-2groups-exp.csv")
write.csv(data.2groups.inc, file="../results/data-2groups-inc.csv")
write.csv(data.unaware, file="../results/data-unaware.csv")




### a) Control items ###
########################

attach(data.2groups)

## Determine the contrasts and calculate the variables that are necessary to make the comparisons
str(data.2groups) 


## Contrast: crit vs. contr (critical vs. control items, across test moment, input, group)

# Obtain mean and sd's of the variables
mean(data.2groups$crit); mean(data.2groups$contr)
sd(data.2groups$crit); sd(data.2groups$contr)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups$crit)
shapiro.test(data.2groups$contr)

qplot(sample=data.2groups$crit, stat="qq")
qplot(sample=data.2groups$contr, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups$crit, data.2groups$contr)

# Obtain the mean of the differences and its SE
data.2groups$crit__contr <- data.2groups$crit - data.2groups$contr # calculate the differences variable (marked by __)
mean(data.2groups$crit__contr) # mean of the differences
std.error(data.2groups$crit__contr) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups$crit__contr, samplemean, R=10000)
plot(b)
boot.ci(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
# (Alternatively, d can also be obtained by means of the ESCI file)
v1 <- data.2groups$crit
v2 <- data.2groups$contr

m1 <- mean(v1); m2 <- mean(v2)  # means
sd1 <- sd(v1); sd2 <- sd(v2)    # sd's
md <- m1-m2                     # mean difference
psd <- sqrt((sd1^2+sd2^2)/2)    # pooled sd
d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_T1 vs. contr_T1 (critical vs. control items at T1, across input, group)

# Obtain mean and sd's of the variables
mean(data.2groups$crit_T1); mean(data.2groups$contr_T1)
sd(data.2groups$crit_T2); sd(data.2groups$contr_T2)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups$crit_T1); qplot(sample=data.2groups$crit_T1, stat="qq")
shapiro.test(data.2groups$contr_T1); qplot(sample=data.2groups$contr_T1, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups$crit_T1, data.2groups$contr_T1)

# Obtain the mean of the differences and its SE
data.2groups$crit_T1__contr_T1 <- data.2groups$crit_T1 - data.2groups$contr_T1 # calculate the differences variable (marked by __)
mean(data.2groups$crit_T1__contr_T1) # mean of the differences
std.error(data.2groups$crit_T1__contr_T1) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups$crit_T1__contr_T1, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups$crit_T1
v2 <- data.2groups$contr_T2
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_T2 vs. contr_T2 (critical vs. control items at T2, across input, group)

# Obtain mean and sd's of the variables
mean(data.2groups$crit_T2); mean(data.2groups$contr_T2)
sd(data.2groups$crit_T2); sd(data.2groups$contr_T2)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups$crit_T2); qplot(sample=data.2groups$crit_T2, stat="qq")
shapiro.test(data.2groups$contr_T2); qplot(sample=data.2groups$contr_T2, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups$crit_T2, data.2groups$contr_T2)

# Obtain the mean of the differences and its SE
data.2groups$crit_T2__contr_T2 <- data.2groups$crit_T2 - data.2groups$contr_T2 # calculate the differences variable (marked by __)
mean(data.2groups$crit_T2__contr_T2) # mean of the differences
std.error(data.2groups$crit_T2__contr_T2) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups$crit_T2__contr_T2, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups$crit_T2
v2 <- data.2groups$contr_T2
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_T3 vs. contr_T3 (critical vs. control items at T1, across input, group)

# Obtain mean and sd's of the variables
mean(data.2groups$crit_T3); mean(data.2groups$contr_T3)
sd(data.2groups$crit_T3); sd(data.2groups$contr_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups$crit_T3); qplot(sample=data.2groups$crit_T3, stat="qq")
shapiro.test(data.2groups$contr_T3); qplot(sample=data.2groups$contr_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups$crit_T3, data.2groups$contr_T3)

# Obtain the mean of the differences and its SE
data.2groups$crit_T3__contr_T3 <- data.2groups$crit_T3 - data.2groups$contr_T3 # calculate the differences variable (marked by __)
mean(data.2groups$crit_T3__contr_T3) # mean of the differences
std.error(data.2groups$crit_T3__contr_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups$crit_T3__contr_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups$crit
v2 <- data.2groups$contr
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE




## Contrast: contr_T1 (across input), grouped by: learning type

# Obtain mean and sd's of the variables
mean(data.2groups.exp$contr_T1); mean(data.2groups.inc$contr_T1)
sd(data.2groups.exp$contr_T1); sd(data.2groups.inc$contr_T1)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.exp$contr_T1); qplot(sample=data.2groups.exp$contr_T1, stat="qq")
shapiro.test(data.2groups.inc$contr_T1); qplot(sample=data.2groups.inc$contr_T1, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.exp$contr_T1, data.2groups.inc$contr_T1)

# Obtain the mean of the differences and its SE
data.2groups.exp$contr_T1_exp__contr_T1_inc <- data.2groups.exp$contr_T1 - data.2groups.inc$contr_T1 # calculate the differences variable (marked by __)
mean(data.2groups.exp$contr_T1_exp__contr_T1_inc) # mean of the differences
std.error(data.2groups.exp$contr_T1_exp__contr_T1_inc) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.exp$contr_T1_exp__contr_T1_inc, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.exp$contr_T1
v2 <- data.2groups.inc$contr_T1
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_input vs. contr_input (across groups, across test moment)

# Obtain mean and sd's of the variables
mean(data.2groups$crit_input); mean(data.2groups$contr_input)
sd(data.2groups$crit_input); sd(data.2groups$contr_input)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups$crit_input); qplot(sample=data.2groups$crit_input, stat="qq")
shapiro.test(data.2groups$contr_input); qplot(sample=data.2groups$contr_input, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups$crit_input, data.2groups$contr_input)

# Obtain the mean of the differences and its SE
data.2groups$crit_input__contr_input <- data.2groups$crit_input - data.2groups$contr_input # calculate the differences variable (marked by __)
mean(data.2groups$crit_input__contr_input) # mean of the differences
std.error(data.2groups$crit_input__contr_input) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups$crit_input__contr_input, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups$crit_input
v2 <- data.2groups$contr_input
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_noinput vs. contr_noinput (across groups, across test moment)

# Obtain mean and sd's of the variables
mean(data.2groups$crit_noinput); mean(data.2groups$contr_noinput)
sd(data.2groups$crit_noinput); sd(data.2groups$contr_noinput)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups$crit_noinput); qplot(sample=data.2groups$crit_noinput, stat="qq")
shapiro.test(data.2groups$contr_noinput); qplot(sample=data.2groups$contr_noinput, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups$crit_noinput, data.2groups$contr_noinput)

# Obtain the mean of the differences and its SE
data.2groups$crit_noinput__contr_noinput <- data.2groups$crit_noinput - data.2groups$contr_noinput # calculate the differences variable (marked by __)
mean(data.2groups$crit_noinput__contr_noinput) # mean of the differences
std.error(data.2groups$crit_noinput__contr_noinput) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups$crit_noinput__contr_noinput, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups$crit_noinput
v2 <- data.2groups$contr_noinput
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: contr_input_T1 vs. contr_input_T2 (explicit)

# Obtain mean and sd's of the variables
mean(data.2groups.exp$contr_input_T1); mean(data.2groups.exp$contr_input_T2)
sd(data.2groups.exp$contr_input_T1); sd(data.2groups.exp$contr_input_T2)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.exp$contr_input_T1); qplot(sample=data.2groups.exp$contr_input_T1, stat="qq")
shapiro.test(data.2groups.exp$contr_input_T2); qplot(sample=data.2groups.exp$contr_input_T2, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.exp$contr_input_T1, data.2groups.exp$contr_input_T2)

# Obtain the mean of the differences and its SE
data.2groups.exp$contr_input_T1__contr_input_T2 <- data.2groups.exp$contr_input_T1 - data.2groups.exp$contr_input_T2 # calculate the differences variable (marked by __)
mean(data.2groups.exp$contr_input_T1__contr_input_T2) # mean of the differences
std.error(data.2groups.exp$contr_input_T1__contr_input_T2) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.exp$contr_input_T1__contr_input_T2, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.exp$contr_input_T1
v2 <- data.2groups.exp$contr_input_T2
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE


## Contrast: contr_noinput_T1 vs. contr_noinput_T2 (explicit)

# Obtain mean and sd's of the variables
mean(data.2groups.exp$contr_noinput_T1); mean(data.2groups.exp$contr_noinput_T2)
sd(data.2groups.exp$contr_noinput_T1); sd(data.2groups.exp$contr_noinput_T2)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.exp$contr_noinput_T1); qplot(sample=data.2groups.exp$contr_noinput_T1, stat="qq")
shapiro.test(data.2groups.exp$contr_noinput_T2); qplot(sample=data.2groups.exp$contr_noinput_T2, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.exp$contr_noinput_T1, data.2groups.exp$contr_noinput_T2)

# Obtain the mean of the differences and its SE
data.2groups.exp$contr_noinput_T1__contr_noinput_T2 <- data.2groups.exp$contr_noinput_T1 - data.2groups.exp$contr_noinput_T2 # calculate the differences variable (marked by __)
mean(data.2groups.exp$contr_noinput_T1__contr_noinput_T2) # mean of the differences
std.error(data.2groups.exp$contr_noinput_T1__contr_noinput_T2) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.exp$contr_noinput_T1__contr_noinput_T2, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.exp$contr_noinput_T1
v2 <- data.2groups.exp$contr_noinput_T2
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: contr_input_T2 vs. contr_input_T3 (explicit)

# Obtain mean and sd's of the variables
mean(data.2groups.exp$contr_input_T2); mean(data.2groups.exp$contr_input_T3)
sd(data.2groups.exp$contr_input_T2); sd(data.2groups.exp$contr_input_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.exp$contr_input_T2); qplot(sample=data.2groups.exp$contr_input_T2, stat="qq")
shapiro.test(data.2groups.exp$contr_input_T3); qplot(sample=data.2groups.exp$contr_input_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.exp$contr_input_T2, data.2groups.exp$contr_input_T3)

# Obtain the mean of the differences and its SE
data.2groups.exp$contr_input_T2__contr_input_T3 <- data.2groups.exp$contr_input_T2 - data.2groups.exp$contr_input_T3 # calculate the differences variable (marked by __)
mean(data.2groups.exp$contr_input_T2__contr_input_T3) # mean of the differences
std.error(data.2groups.exp$contr_input_T2__contr_input_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.exp$contr_input_T2__contr_input_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.exp$contr_input_T2
v2 <- data.2groups.exp$contr_input_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: contr_noinput_T2 vs. contr_noinput_T3 (explicit)

# Obtain mean and sd's of the variables
mean(data.2groups.exp$contr_noinput_T2); mean(data.2groups.exp$contr_noinput_T3)
sd(data.2groups.exp$contr_noinput_T2); sd(data.2groups.exp$contr_noinput_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.exp$contr_noinput_T2); qplot(sample=data.2groups.exp$contr_noinput_T2, stat="qq")
shapiro.test(data.2groups.exp$contr_noinput_T3); qplot(sample=data.2groups.exp$contr_noinput_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.exp$contr_noinput_T2, data.2groups.exp$contr_noinput_T3)

# Obtain the mean of the differences and its SE
data.2groups.exp$contr_noinput_T2__contr_noinput_T3 <- data.2groups.exp$contr_noinput_T2 - data.2groups.exp$contr_noinput_T3 # calculate the differences variable (marked by __)
mean(data.2groups.exp$contr_noinput_T2__contr_noinput_T3) # mean of the differences
std.error(data.2groups.exp$contr_noinput_T2__contr_noinput_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.exp$contr_noinput_T2__contr_noinput_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.exp$contr_noinput_T2
v2 <- data.2groups.exp$contr_noinput_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: contr_T1 vs. contr_T2 (incidental)

# Obtain mean and sd's of the variables
mean(data.2groups.inc$contr_T1); mean(data.2groups.inc$contr_T2)
sd(data.2groups.inc$contr_T1); sd(data.2groups.inc$contr_T2)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.inc$contr_T1); qplot(sample=data.2groups.inc$contr_T1, stat="qq")
shapiro.test(data.2groups.inc$contr_T2); qplot(sample=data.2groups.inc$contr_T2, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.inc$contr_T1, data.2groups.inc$contr_T2)

# Obtain the mean of the differences and its SE
data.2groups.inc$contr_T1__contr_T2 <- data.2groups.inc$contr_T1 - data.2groups.inc$contr_T2 # calculate the differences variable (marked by __)
mean(data.2groups.inc$contr_T1__contr_T2) # mean of the differences
std.error(data.2groups.inc$contr_T1__contr_T2) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.inc$contr_T1__contr_T2, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.inc$contr_T1
v2 <- data.2groups.inc$contr_T2
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: contr_T2 vs. contr_T3 (incidental)

# Obtain mean and sd's of the variables
mean(data.2groups.inc$contr_T2); mean(data.2groups.inc$contr_T3)
sd(data.2groups.inc$contr_T2); sd(data.2groups.inc$contr_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.inc$contr_T2); qplot(sample=data.2groups.inc$contr_T2, stat="qq")
shapiro.test(data.2groups.inc$contr_T3); qplot(sample=data.2groups.inc$contr_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.inc$contr_T2, data.2groups.inc$contr_T3)

# Obtain the mean of the differences and its SE
data.2groups.inc$contr_T2__contr_T3 <- data.2groups.inc$contr_T2 - data.2groups.inc$contr_T3 # calculate the differences variable (marked by __)
mean(data.2groups.inc$contr_T2__contr_T3) # mean of the differences
std.error(data.2groups.inc$contr_T2__contr_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.inc$contr_T2__contr_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.inc$contr_T2
v2 <- data.2groups.inc$contr_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: contr_input vs. contr_noinput (incidental)

# Obtain mean and sd's of the variables
mean(data.2groups.inc$contr_input); mean(data.2groups.inc$contr_noinput)
sd(data.2groups.inc$contr_input); sd(data.2groups.inc$contr_noinput)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.inc$contr_input); qplot(sample=data.2groups.inc$contr_input, stat="qq")
shapiro.test(data.2groups.inc$contr_noinput); qplot(sample=data.2groups.inc$contr_noinput, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.inc$contr_input, data.2groups.inc$contr_noinput)

# Obtain the mean of the differences and its SE
data.2groups.inc$contr_input__contr_noinput <- data.2groups.inc$contr_input - data.2groups.inc$contr_noinput # calculate the differences variable (marked by __)
mean(data.2groups.inc$contr_input__contr_noinput) # mean of the differences
std.error(data.2groups.inc$contr_input__contr_noinput) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.inc$contr_input__contr_noinput, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.inc$contr_input
v2 <- data.2groups.inc$contr_noinput
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE




### b) T3 (explicit posttest) ###
#################################

## Determine the contrasts

## Contrast: crit_input_T2 vs. crit_input_T3 (explicit group)
## Contrast: crit_noinput_T2 vs. crit_noinput_T3 (explicit group)
## Contrast: crit_input_T2 vs. crit_input_T3 (incidental group)
## Contrast: crit_noinput_T2 vs. crit_noinput_T3 (incidental group)

## Contrast: crit_input_T2 (explicit vs. incidental)
## Contrast: crit_noinput_T2 (explicit vs. incidental)
## Contrast: crit_input_T3 (explicit vs. incidental)
## Contrast: crit_noinput_T3 (explicit vs. incidental)

## Contrast: crit_input_T2 vs. crit_noinput_T2 (explicit group)
## Contrast: crit_input_T3 vs. crit_noinput_T3 (explicit group)
## Contrast: crit_input_T2 vs. crit_noinput_T2 (incidental group)
## Contrast: crit_input_T3 vs. crit_noinput_T3 (incidental group)



## Contrast: crit_input_T2 vs. crit_input_T3 (explicit group)

# Obtain mean and sd's of the variables
mean(data.2groups.exp$crit_input_T2); mean(data.2groups.exp$crit_input_T3)
sd(data.2groups.exp$crit_input_T2); sd(data.2groups.exp$crit_input_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.exp$crit_input_T2); qplot(sample=data.2groups.exp$crit_input_T2, stat="qq")
shapiro.test(data.2groups.exp$crit_input_T3); qplot(sample=data.2groups.exp$crit_input_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.exp$crit_input_T2, data.2groups.exp$crit_input_T3)

# Obtain the mean of the differences and its SE
data.2groups.exp$crit_input_T2__crit_input_T3 <- data.2groups.exp$crit_input_T2 - data.2groups.exp$crit_input_T3 # calculate the differences variable (marked by __)
mean(data.2groups.exp$crit_input_T2__crit_input_T3) # mean of the differences
std.error(data.2groups.exp$crit_input_T2__crit_input_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.exp$crit_input_T2__crit_input_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.exp$crit_input_T2
v2 <- data.2groups.exp$crit_input_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_noinput_T2 vs. crit_noinput_T3 (explicit group)

# Obtain mean and sd's of the variables
mean(data.2groups.exp$crit_noinput_T2); mean(data.2groups.exp$crit_noinput_T3)
sd(data.2groups.exp$crit_noinput_T2); sd(data.2groups.exp$crit_noinput_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.exp$crit_noinput_T2); qplot(sample=data.2groups.exp$crit_noinput_T2, stat="qq")
shapiro.test(data.2groups.exp$crit_noinput_T3); qplot(sample=data.2groups.exp$crit_noinput_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.exp$crit_noinput_T2, data.2groups.exp$crit_noinput_T3)

# Obtain the mean of the differences and its SE
data.2groups.exp$crit_noinput_T2__crit_noinput_T3 <- data.2groups.exp$crit_noinput_T2 - data.2groups.exp$crit_noinput_T3 # calculate the differences variable (marked by __)
mean(data.2groups.exp$crit_noinput_T2__crit_noinput_T3) # mean of the differences
std.error(data.2groups.exp$crit_noinput_T2__crit_noinput_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.exp$crit_noinput_T2__crit_noinput_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.exp$crit_noinput_T2
v2 <- data.2groups.exp$crit_noinput_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d



## Contrast: crit_input_T2 vs. crit_input_T3 (incidental group)

# Obtain mean and sd's of the variables
mean(data.2groups.inc$crit_input_T2); mean(data.2groups.inc$crit_input_T3)
sd(data.2groups.inc$crit_input_T2); sd(data.2groups.inc$crit_input_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.inc$crit_input_T2); qplot(sample=data.2groups.inc$crit_input_T2, stat="qq")
shapiro.test(data.2groups.inc$crit_input_T3); qplot(sample=data.2groups.inc$crit_input_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.inc$crit_input_T2, data.2groups.inc$crit_input_T3)

# Obtain the mean of the differences and its SE
data.2groups.inc$crit_input_T2__crit_input_T3 <- data.2groups.inc$crit_input_T2 - data.2groups.inc$crit_input_T3 # calculate the differences variable (marked by __)
mean(data.2groups.inc$crit_input_T2__crit_input_T3) # mean of the differences
std.error(data.2groups.inc$crit_input_T2__crit_input_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.inc$crit_input_T2__crit_input_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.inc$crit_input_T2
v2 <- data.2groups.inc$crit_input_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_noinput_T2 vs. crit_noinput_T3 (incidental group)

# Obtain mean and sd's of the variables
mean(data.2groups.inc$crit_noinput_T2); mean(data.2groups.inc$crit_noinput_T3)
sd(data.2groups.inc$crit_noinput_T2); sd(data.2groups.inc$crit_noinput_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.inc$crit_noinput_T2); qplot(sample=data.2groups.inc$crit_noinput_T2, stat="qq")
shapiro.test(data.2groups.inc$crit_noinput_T3); qplot(sample=data.2groups.inc$crit_noinput_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.inc$crit_noinput_T2, data.2groups.inc$crit_noinput_T3)

# Obtain the mean of the differences and its SE
data.2groups.inc$crit_noinput_T2__crit_noinput_T3 <- data.2groups.inc$crit_noinput_T2 - data.2groups.inc$crit_noinput_T3 # calculate the differences variable (marked by __)
mean(data.2groups.inc$crit_noinput_T2__crit_noinput_T3) # mean of the differences
std.error(data.2groups.inc$crit_noinput_T2__crit_noinput_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.inc$crit_noinput_T2__crit_noinput_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.inc$crit_noinput_T2
v2 <- data.2groups.inc$crit_noinput_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d



## Contrast: crit_input_T2 (explicit vs. incidental)

# Obtain mean and sd's of the variables
mean(data.2groups.exp$crit_input_T2); mean(data.2groups.inc$crit_input_T2)
sd(data.2groups.exp$crit_input_T2); sd(data.2groups.inc$crit_input_T2)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.exp$crit_input_T2); qplot(sample=data.2groups.exp$crit_input_T2, stat="qq")
shapiro.test(data.2groups.inc$crit_input_T2); qplot(sample=data.2groups.inc$crit_input_T2, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.exp$crit_input_T2, data.2groups.inc$crit_input_T2)

# Obtain the mean of the differences and its SE
data.2groups.exp$crit_input_T2_exp__crit_input_T2_inc <- data.2groups.exp$crit_input_T2 - data.2groups.inc$crit_input_T2 # calculate the differences variable (marked by __)
mean(data.2groups.exp$crit_input_T2_exp__crit_input_T2_inc) # mean of the differences
std.error(data.2groups.exp$crit_input_T2_exp__crit_input_T2_inc) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.exp$crit_input_T2_exp__crit_input_T2_inc, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.exp$crit_input_T2
v2 <- data.2groups.inc$crit_input_T2
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_noinput_T2 (explicit vs. incidental)

# Obtain mean and sd's of the variables
mean(data.2groups.exp$crit_noinput_T2); mean(data.2groups.inc$crit_noinput_T2)
sd(data.2groups.exp$crit_noinput_T2); sd(data.2groups.inc$crit_noinput_T2)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.exp$crit_noinput_T2); qplot(sample=data.2groups.exp$crit_noinput_T2, stat="qq")
shapiro.test(data.2groups.inc$crit_noinput_T2); qplot(sample=data.2groups.inc$crit_noinput_T2, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.exp$crit_noinput_T2, data.2groups.inc$crit_noinput_T2)

# Obtain the mean of the differences and its SE
data.2groups.exp$crit_noinput_T2_exp__crit_noinput_T2_inc <- data.2groups.exp$crit_noinput_T2 - data.2groups.inc$crit_noinput_T2 # calculate the differences variable (marked by __)
mean(data.2groups.exp$crit_noinput_T2_exp__crit_noinput_T2_inc) # mean of the differences
std.error(data.2groups.exp$crit_noinput_T2_exp__crit_noinput_T2_inc) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.exp$crit_noinput_T2_exp__crit_noinput_T2_inc, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.exp$crit_noinput_T2
v2 <- data.2groups.inc$crit_noinput_T2
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_input_T3 (explicit vs. incidental)

# Obtain mean and sd's of the variables
mean(data.2groups.exp$crit_input_T3); mean(data.2groups.inc$crit_input_T3)
sd(data.2groups.exp$crit_input_T3); sd(data.2groups.inc$crit_input_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.exp$crit_input_T3); qplot(sample=data.2groups.exp$crit_input_T3, stat="qq")
shapiro.test(data.2groups.inc$crit_input_T3); qplot(sample=data.2groups.inc$crit_input_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.exp$crit_input_T3, data.2groups.inc$crit_input_T3)

# Obtain the mean of the differences and its SE
data.2groups.exp$crit_input_T3_exp__crit_input_T3_inc <- data.2groups.exp$crit_input_T3 - data.2groups.inc$crit_input_T3 # calculate the differences variable (marked by __)
mean(data.2groups.exp$crit_input_T3_exp__crit_input_T3_inc) # mean of the differences
std.error(data.2groups.exp$crit_input_T3_exp__crit_input_T3_inc) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.exp$crit_input_T3_exp__crit_input_T3_inc, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.exp$crit_input_T3
v2 <- data.2groups.inc$crit_input_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_noinput_T3 (explicit vs. incidental)

# Obtain mean and sd's of the variables
mean(data.2groups.exp$crit_noinput_T3); mean(data.2groups.inc$crit_noinput_T3)
sd(data.2groups.exp$crit_noinput_T3); sd(data.2groups.inc$crit_noinput_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.exp$crit_noinput_T3); qplot(sample=data.2groups.exp$crit_noinput_T3, stat="qq")
shapiro.test(data.2groups.inc$crit_noinput_T3); qplot(sample=data.2groups.inc$crit_noinput_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.exp$crit_noinput_T3, data.2groups.inc$crit_noinput_T3)

# Obtain the mean of the differences and its SE
data.2groups.exp$crit_noinput_T3_exp__crit_noinput_T3_inc <- data.2groups.exp$crit_noinput_T3 - data.2groups.inc$crit_noinput_T3 # calculate the differences variable (marked by __)
mean(data.2groups.exp$crit_noinput_T3_exp__crit_noinput_T3_inc) # mean of the differences
std.error(data.2groups.exp$crit_noinput_T3_exp__crit_noinput_T3_inc) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.exp$crit_noinput_T3_exp__crit_noinput_T3_inc, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.exp$crit_noinput_T3
v2 <- data.2groups.inc$crit_noinput_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_input_T2 vs. crit_noinput_T2 (explicit group)

# Obtain mean and sd's of the variables
mean(data.2groups.exp$crit_input_T2); mean(data.2groups.exp$crit_noinput_T2)
sd(data.2groups.exp$crit_input_T2); sd(data.2groups.exp$crit_noinput_T2)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.exp$crit_input_T2); qplot(sample=data.2groups.exp$crit_input_T2, stat="qq")
shapiro.test(data.2groups.exp$crit_noinput_T2); qplot(sample=data.2groups.exp$crit_noinput_T2, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.exp$crit_input_T2, data.2groups.exp$crit_noinput_T2)

# Obtain the mean of the differences and its SE
data.2groups.exp$crit_input_T2__crit_noinput_T2 <- data.2groups.exp$crit_input_T2 - data.2groups.exp$crit_noinput_T2 # calculate the differences variable (marked by __)
mean(data.2groups.exp$crit_input_T2__crit_noinput_T2) # mean of the differences
std.error(data.2groups.exp$crit_input_T2__crit_noinput_T2) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.exp$crit_input_T2__crit_noinput_T2, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.exp$crit_input_T2
v2 <- data.2groups.exp$crit_noinput_T2
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_input_T3 vs. crit_noinput_T3 (explicit group)

# Obtain mean and sd's of the variables
mean(data.2groups.exp$crit_input_T3); mean(data.2groups.exp$crit_noinput_T3)
sd(data.2groups.exp$crit_input_T3); sd(data.2groups.exp$crit_noinput_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.exp$crit_input_T3); qplot(sample=data.2groups.exp$crit_input_T3, stat="qq")
shapiro.test(data.2groups.exp$crit_noinput_T3); qplot(sample=data.2groups.exp$crit_noinput_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.exp$crit_input_T3, data.2groups.exp$crit_noinput_T3)

# Obtain the mean of the differences and its SE
data.2groups.exp$crit_input_T3__crit_noinput_T3 <- data.2groups.exp$crit_input_T3 - data.2groups.exp$crit_noinput_T3 # calculate the differences variable (marked by __)
mean(data.2groups.exp$crit_input_T3__crit_noinput_T3) # mean of the differences
std.error(data.2groups.exp$crit_input_T3__crit_noinput_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.exp$crit_input_T3__crit_noinput_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.exp$crit_input_T3
v2 <- data.2groups.exp$crit_noinput_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_input_T2__crit_noinput_T2 vs. crit_input_T3__crit_noinput_T3 (explicit group)

# Obtain mean and sd's of the variables
mean(data.2groups.exp$crit_input_T2__crit_noinput_T2); mean(data.2groups.exp$crit_input_T3__crit_noinput_T3)
sd(data.2groups.exp$crit_input_T2__crit_noinput_T2); sd(data.2groups.exp$crit_input_T3__crit_noinput_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.exp$crit_input_T2__crit_noinput_T2); qplot(sample=data.2groups.exp$crit_input_T2__crit_noinput_T2, stat="qq")
shapiro.test(data.2groups.exp$crit_input_T3__crit_noinput_T3); qplot(sample=data.2groups.exp$crit_input_T3__crit_noinput_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.exp$crit_input_T2__crit_noinput_T2, data.2groups.exp$crit_input_T3__crit_noinput_T3)

# Obtain the mean of the differences and its SE
data.2groups.exp$crit_input_T2__crit_noinput_T2__crit_input_T3__crit_noinput_T3 <- data.2groups.exp$crit_input_T2__crit_noinput_T2 - data.2groups.exp$crit_input_T3__crit_noinput_T3 # calculate the differences variable (marked by __)
mean(data.2groups.exp$crit_input_T2__crit_noinput_T2__crit_input_T3__crit_noinput_T3) # mean of the differences
std.error(data.2groups.exp$crit_input_T2__crit_noinput_T2__crit_input_T3__crit_noinput_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.exp$crit_input_T2__crit_noinput_T2__crit_input_T3__crit_noinput_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.exp$crit_input_T2__crit_noinput_T2
v2 <- data.2groups.exp$crit_input_T3__crit_noinput_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_input_T2__crit_noinput_T2 vs. crit_input_T3__crit_noinput_T3 (incidental group)

# Obtain mean and sd's of the variables
mean(data.2groups.inc$crit_input_T2__crit_noinput_T2); mean(data.2groups.inc$crit_input_T3__crit_noinput_T3)
sd(data.2groups.inc$crit_input_T2__crit_noinput_T2); sd(data.2groups.inc$crit_input_T3__crit_noinput_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.inc$crit_input_T2__crit_noinput_T2); qplot(sample=data.2groups.inc$crit_input_T2__crit_noinput_T2, stat="qq")
shapiro.test(data.2groups.inc$crit_input_T3__crit_noinput_T3); qplot(sample=data.2groups.inc$crit_input_T3__crit_noinput_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.inc$crit_input_T2__crit_noinput_T2, data.2groups.inc$crit_input_T3__crit_noinput_T3)

# Obtain the mean of the differences and its SE
data.2groups.inc$crit_input_T2__crit_noinput_T2__crit_input_T3__crit_noinput_T3 <- data.2groups.inc$crit_input_T2__crit_noinput_T2 - data.2groups.inc$crit_input_T3__crit_noinput_T3 # calculate the differences variable (marked by __)
mean(data.2groups.inc$crit_input_T2__crit_noinput_T2__crit_input_T3__crit_noinput_T3) # mean of the differences
std.error(data.2groups.inc$crit_input_T2__crit_noinput_T2__crit_input_T3__crit_noinput_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.inc$crit_input_T2__crit_noinput_T2__crit_input_T3__crit_noinput_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.inc$crit_input_T2__crit_noinput_T2
v2 <- data.2groups.inc$crit_input_T3__crit_noinput_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d



## Contrast: crit_input_T2 vs. crit_noinput_T2 (incidental group)

# Obtain mean and sd's of the variables
mean(data.2groups.inc$crit_input_T2); mean(data.2groups.inc$crit_noinput_T2)
sd(data.2groups.inc$crit_input_T2); sd(data.2groups.inc$crit_noinput_T2)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.inc$crit_input_T2); qplot(sample=data.2groups.inc$crit_input_T2, stat="qq")
shapiro.test(data.2groups.inc$crit_noinput_T2); qplot(sample=data.2groups.inc$crit_noinput_T2, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.inc$crit_input_T2, data.2groups.inc$crit_noinput_T2)

# Obtain the mean of the differences and its SE
data.2groups.inc$crit_input_T2__crit_noinput_T2 <- data.2groups.inc$crit_input_T2 - data.2groups.inc$crit_noinput_T2 # calculate the differences variable (marked by __)
mean(data.2groups.inc$crit_input_T2__crit_noinput_T2) # mean of the differences
std.error(data.2groups.inc$crit_input_T2__crit_noinput_T2) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.inc$crit_input_T2__crit_noinput_T2, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.inc$crit_input_T2
v2 <- data.2groups.inc$crit_noinput_T2
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_input_T3 vs. crit_noinput_T3 (incidental group)

# Obtain mean and sd's of the variables
mean(data.2groups.inc$crit_input_T3); mean(data.2groups.inc$crit_noinput_T3)
sd(data.2groups.inc$crit_input_T3); sd(data.2groups.inc$crit_noinput_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.2groups.inc$crit_input_T3); qplot(sample=data.2groups.inc$crit_input_T3, stat="qq")
shapiro.test(data.2groups.inc$crit_noinput_T3); qplot(sample=data.2groups.inc$crit_noinput_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.2groups.inc$crit_input_T3, data.2groups.inc$crit_noinput_T3)

# Obtain the mean of the differences and its SE
data.2groups.inc$crit_input_T3__crit_noinput_T3 <- data.2groups.inc$crit_input_T3 - data.2groups.inc$crit_noinput_T3 # calculate the differences variable (marked by __)
mean(data.2groups.inc$crit_input_T3__crit_noinput_T3) # mean of the differences
std.error(data.2groups.inc$crit_input_T3__crit_noinput_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.2groups.inc$crit_input_T3__crit_noinput_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.2groups.inc$crit_input_T3
v2 <- data.2groups.inc$crit_noinput_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE





### c) Unaware group [-TS,-LP] ###
##################################

mean(data.unaware$crit_T1_T2)
sd(data.unaware$crit_T1_T2)

mean(data.unaware$contr_T1_T2)
sd(data.unaware$contr_T1_T2)


## Contrast: crit_input_T1 vs. crit_input_T2

# Obtain mean and sd's of the variables
mean(data.unaware$crit_input_T1); mean(data.unaware$crit_input_T2)
sd(data.unaware$crit_input_T1); sd(data.unaware$crit_input_T2)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.unaware$crit_input_T1); qplot(sample=data.unaware$crit_input_T1, stat="qq")
shapiro.test(data.unaware$crit_input_T2); qplot(sample=data.unaware$crit_input_T2, stat="qq")

# Assess homogeneity of variances
fligner.test(data.unaware$crit_input_T1, data.unaware$crit_input_T2)

# Obtain the mean of the differences and its SE
data.unaware$crit_input_T1__crit_input_T2 <- data.unaware$crit_input_T1 - data.unaware$crit_input_T2 # calculate the differences variable (marked by __)
mean(data.unaware$crit_input_T1__crit_input_T2) # mean of the differences
std.error(data.unaware$crit_input_T1__crit_input_T2) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.unaware$crit_input_T1__crit_input_T2, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.unaware$crit_input_T1
v2 <- data.unaware$crit_input_T2
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_noinput_T1 vs. crit_noinput_T2

# Obtain mean and sd's of the variables
mean(data.unaware$crit_noinput_T1); mean(data.unaware$crit_noinput_T2)
sd(data.unaware$crit_noinput_T1); sd(data.unaware$crit_noinput_T2)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.unaware$crit_noinput_T1); qplot(sample=data.unaware$crit_noinput_T1, stat="qq")
shapiro.test(data.unaware$crit_noinput_T2); qplot(sample=data.unaware$crit_noinput_T2, stat="qq")

# Assess homogeneity of variances
fligner.test(data.unaware$crit_noinput_T1, data.unaware$crit_noinput_T2)

# Obtain the mean of the differences and its SE
data.unaware$crit_noinput_T1__crit_noinput_T2 <- data.unaware$crit_noinput_T1 - data.unaware$crit_noinput_T2 # calculate the differences variable (marked by __)
mean(data.unaware$crit_noinput_T1__crit_noinput_T2) # mean of the differences
std.error(data.unaware$crit_noinput_T1__crit_noinput_T2) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.unaware$crit_noinput_T1__crit_noinput_T2, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.unaware$crit_noinput_T1
v2 <- data.unaware$crit_noinput_T2
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_input_T2 vs. crit_input_T3

# Obtain mean and sd's of the variables
mean(data.unaware$crit_input_T2); mean(data.unaware$crit_input_T3)
sd(data.unaware$crit_input_T2); sd(data.unaware$crit_input_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.unaware$crit_input_T2); qplot(sample=data.unaware$crit_input_T2, stat="qq")
shapiro.test(data.unaware$crit_input_T3); qplot(sample=data.unaware$crit_input_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.unaware$crit_input_T2, data.unaware$crit_input_T3)

# Obtain the mean of the differences and its SE
data.unaware$crit_input_T2__crit_input_T3 <- data.unaware$crit_input_T2 - data.unaware$crit_input_T3 # calculate the differences variable (marked by __)
mean(data.unaware$crit_input_T2__crit_input_T3) # mean of the differences
std.error(data.unaware$crit_input_T2__crit_input_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.unaware$crit_input_T2__crit_input_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.unaware$crit_input_T2
v2 <- data.unaware$crit_input_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: crit_noinput_T2 vs. crit_noinput_T3

# Obtain mean and sd's of the variables
mean(data.unaware$crit_noinput_T2); mean(data.unaware$crit_noinput_T3)
sd(data.unaware$crit_noinput_T2); sd(data.unaware$crit_noinput_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.unaware$crit_noinput_T2); qplot(sample=data.unaware$crit_noinput_T2, stat="qq")
shapiro.test(data.unaware$crit_noinput_T3); qplot(sample=data.unaware$crit_noinput_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.unaware$crit_noinput_T2, data.unaware$crit_noinput_T3)

# Obtain the mean of the differences and its SE
data.unaware$crit_noinput_T2__crit_noinput_T3 <- data.unaware$crit_noinput_T2 - data.unaware$crit_noinput_T3 # calculate the differences variable (marked by __)
mean(data.unaware$crit_noinput_T2__crit_noinput_T3) # mean of the differences
std.error(data.unaware$crit_noinput_T2__crit_noinput_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.unaware$crit_noinput_T2__crit_noinput_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.unaware$crit_noinput_T2
v2 <- data.unaware$crit_noinput_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: contr_input_T1 vs. contr_input_T2

# Obtain mean and sd's of the variables
mean(data.unaware$contr_input_T1); mean(data.unaware$contr_input_T2)
sd(data.unaware$contr_input_T1); sd(data.unaware$contr_input_T2)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.unaware$contr_input_T1); qplot(sample=data.unaware$contr_input_T1, stat="qq")
shapiro.test(data.unaware$contr_input_T2); qplot(sample=data.unaware$contr_input_T2, stat="qq")

# Assess homogeneity of variances
fligner.test(data.unaware$contr_input_T1, data.unaware$contr_input_T2)

# Obtain the mean of the differences and its SE
data.unaware$contr_input_T1__contr_input_T2 <- data.unaware$contr_input_T1 - data.unaware$contr_input_T2 # calculate the differences variable (marked by __)
mean(data.unaware$contr_input_T1__contr_input_T2) # mean of the differences
std.error(data.unaware$contr_input_T1__contr_input_T2) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.unaware$contr_input_T1__contr_input_T2, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.unaware$contr_input_T1
v2 <- data.unaware$contr_input_T2
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: contr_noinput_T1 vs. contr_noinput_T2

# Obtain mean and sd's of the variables
mean(data.unaware$contr_noinput_T1); mean(data.unaware$contr_noinput_T2)
sd(data.unaware$contr_noinput_T1); sd(data.unaware$contr_noinput_T2)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.unaware$contr_noinput_T1); qplot(sample=data.unaware$contr_noinput_T1, stat="qq")
shapiro.test(data.unaware$contr_noinput_T2); qplot(sample=data.unaware$contr_noinput_T2, stat="qq")

# Assess homogeneity of variances
fligner.test(data.unaware$contr_noinput_T1, data.unaware$contr_noinput_T2)

# Obtain the mean of the differences and its SE
data.unaware$contr_noinput_T1__contr_noinput_T2 <- data.unaware$contr_noinput_T1 - data.unaware$contr_noinput_T2 # calculate the differences variable (marked by __)
mean(data.unaware$contr_noinput_T1__contr_noinput_T2) # mean of the differences
std.error(data.unaware$contr_noinput_T1__contr_noinput_T2) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.unaware$contr_noinput_T1__contr_noinput_T2, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.unaware$contr_noinput_T1
v2 <- data.unaware$contr_noinput_T2
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: contr_input_T2 vs. contr_input_T3

# Obtain mean and sd's of the variables
mean(data.unaware$contr_input_T2); mean(data.unaware$contr_input_T3)
sd(data.unaware$contr_input_T2); sd(data.unaware$contr_input_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.unaware$contr_input_T2); qplot(sample=data.unaware$contr_input_T2, stat="qq")
shapiro.test(data.unaware$contr_input_T3); qplot(sample=data.unaware$contr_input_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.unaware$contr_input_T2, data.unaware$contr_input_T3)

# Obtain the mean of the differences and its SE
data.unaware$contr_input_T2__contr_input_T3 <- data.unaware$contr_input_T2 - data.unaware$contr_input_T3 # calculate the differences variable (marked by __)
mean(data.unaware$contr_input_T2__contr_input_T3) # mean of the differences
std.error(data.unaware$contr_input_T2__contr_input_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.unaware$contr_input_T2__contr_input_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.unaware$contr_input_T2
v2 <- data.unaware$contr_input_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: contr_noinput_T2 vs. contr_noinput_T3

# Obtain mean and sd's of the variables
mean(data.unaware$contr_noinput_T2); mean(data.unaware$contr_noinput_T3)
sd(data.unaware$contr_noinput_T2); sd(data.unaware$contr_noinput_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.unaware$contr_noinput_T2); qplot(sample=data.unaware$contr_noinput_T2, stat="qq")
shapiro.test(data.unaware$contr_noinput_T3); qplot(sample=data.unaware$contr_noinput_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.unaware$contr_noinput_T2, data.unaware$contr_noinput_T3)

# Obtain the mean of the differences and its SE
data.unaware$contr_noinput_T2__contr_noinput_T3 <- data.unaware$contr_noinput_T2 - data.unaware$contr_noinput_T3 # calculate the differences variable (marked by __)
mean(data.unaware$contr_noinput_T2__contr_noinput_T3) # mean of the differences
std.error(data.unaware$contr_noinput_T2__contr_noinput_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.unaware$contr_noinput_T2__contr_noinput_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.unaware$contr_noinput_T2
v2 <- data.unaware$contr_noinput_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE



## Contrast: contr_T2 vs. contr_T3

# Obtain mean and sd's of the variables
mean(data.unaware$contr_T2); mean(data.unaware$contr_T3)
sd(data.unaware$contr_T2); sd(data.unaware$contr_T3)

# Assess normality: Shapiro-Wilk test (should not be significant)
shapiro.test(data.unaware$contr_T2); qplot(sample=data.unaware$contr_T2, stat="qq")
shapiro.test(data.unaware$contr_T3); qplot(sample=data.unaware$contr_T3, stat="qq")

# Assess homogeneity of variances
fligner.test(data.unaware$contr_T2, data.unaware$contr_T3)

# Obtain the mean of the differences and its SE
data.unaware$contr_T2__contr_T3 <- data.unaware$contr_T2 - data.unaware$contr_T3 # calculate the differences variable (marked by __)
mean(data.unaware$contr_T2__contr_T3) # mean of the differences
std.error(data.unaware$contr_T2__contr_T3) # SE for the mean difference

# Boostrapped CI
samplemean <- function(x, d){return(mean(x[d]))}
b=boot(data.unaware$contr_T2__contr_T3, samplemean, R=10000)
boot.ci(b); plot(b)

# Calculate the effect size (Cohen's d) (the standardizer is the pooled sd)
v1 <- data.unaware$contr_T2
v2 <- data.unaware$contr_T3
m1 <- mean(v1); m2 <- mean(v2); sd1 <- sd(v1); sd2 <- sd(v2); md <- m1-m2; psd <- sqrt((sd1^2+sd2^2)/2); d <- md/psd
d

# Output of the ESCI file: d CIs, d MOE
