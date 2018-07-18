# Copyright Eva Koch (2018)

# Script for Study 1, created in May 2018.
# The script was written in order to get the descriptive statistics. It uses the input data file in the wide format, containing
# the calculated test scores (percentages) for each participant.

# Set working directory to source file location (from https://eranraviv.com/r-tips-and-tricks-working-directory/)
library(rstudioapi)
set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path # get the path of your current open file
  setwd(dirname(current_path )) # set the working directory to the source file location
  print( getwd() ) # display the directory
}
set_wd()

rm(list=ls()) # clear workspace

options(contrasts=c("contr.sum","contr.poly")) # Set contrasts to sum-to-zero



## Data preparation
###################

# Load data
participantsdata <- read.csv("../data/Data_participants_wide.csv", header = TRUE) # load data
colnames(participantsdata)[1] <- "participant" # rename participant variable
str(participantsdata)

# Turn self-rated variables into ordered factor:
proficiency_overall <- factor(participantsdata$proficiency_overall, ordered=TRUE)
proficiency_written <- factor(participantsdata$proficiency_written, ordered=TRUE)
proficiency_oral <- factor(participantsdata$proficiency_oral, ordered=TRUE)
comprehension_oral <- factor(participantsdata$comprehension_oral, ordered=TRUE)
comprehension_written <- factor(participantsdata$comprehension_written, ordered=TRUE)
usage <- factor(participantsdata$usage, ordered=TRUE)
  # (Info: Scientists often treat likert scale data as interval data, but this practice is questionable. We decided to treat
  # our 5 point likert scale data as ordinal. Oridnal = categorical, when the categories are ordered. This is also recommended
  # by Field, Miles & Field, 2012, p.9; Jane Superbrain box 1.2).

# Create subsets (per learningtype):
pp.explicit <- subset(participantsdata, subset=learningtype=="explicit")
pp.incidental <- subset(participantsdata, subset=learningtype=="incidental")
pp.unaware <- subset(participantsdata, subset=learningtype=="unaware")

# Create a dataset for explicit/incidental groups only (exclude unaware)
pp.explicit.incidental <- subset(participantsdata, learningtype != "unaware") # drop the "unaware" learners from the dataset
pp.explicit.incidental$learningtype <- factor(pp.explicit.incidental$learningtype) # turn the same factor into a factor again in order to drop the unused "unaware" level
levels(pp.explicit.incidental$learningtype) # the factor has now only two levels.


## Obtain the descriptive statistics for the test scores
########################################################

library(fBasics) # necessary package (attention: only works with numerical data)
colnames(participantsdata) # to get variable overview
library(data.table) # needed to keep the row names when we save the outcomes in objects

# Info: We ask for the descriptives for columns 28-39 (= columns with test scores). We do this for the full dataset ("all"), as well
# as for the three learningtype groups ("exp", "inc", "una") separately. "all2" refers to descriptives across the explicit and incidental,
# but not the unaware groups.
scores.all <- basicStats(participantsdata[, 28:39]); setDT(scores.all, keep.rownames = TRUE); colnames(scores.all)[1] <- "descriptives"
scores.exp <- basicStats(pp.explicit[, 28:39]); setDT(scores.exp, keep.rownames = TRUE); colnames(scores.exp)[1] <- "descriptives"
scores.inc <- basicStats(pp.incidental[, 28:39]); setDT(scores.inc, keep.rownames = TRUE); colnames(scores.inc)[1] <- "descriptives"
scores.una <- basicStats(pp.unaware[, 28:39]); setDT(scores.una, keep.rownames = TRUE); colnames(scores.una)[1] <- "descriptives"
scores.all2 <- basicStats(pp.explicit.incidental[, 28:39]); setDT(scores.all2 , keep.rownames = TRUE); colnames(scores.all2 )[1] <- "descriptives"

# Dubbel-checl the CI
t.test(pp.explicit$crit_input_T1, conf.level = 0.95)$conf.int # Gives the same result as the basicStats function.


# Obtain bootstrapped CI (10.000 samples)

library(boot)
# Source: Larson-Hall 2016, p.97.
# Output interpretation: Check the plot: does the bootstrap look like a normal distribution?; Report the adjusted bootstrap percentile (BCa) interval.

samplemean <- function(x, d){
  return(mean(x[d]))
}

# # critical items # # 
# explicit
b=boot(pp.explicit$crit_input_T1, samplemean, R=10000)
plot(b)
boot.ci(b)

b=boot(pp.explicit$crit_input_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit$crit_input_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit$crit_noinput_T1, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit$crit_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit$crit_noinput_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

# incidental
b=boot(pp.incidental$crit_input_T1, samplemean, R=10000)
plot(b)
boot.ci(b)

b=boot(pp.incidental$crit_input_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.incidental$crit_input_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.incidental$crit_noinput_T1, samplemean, R=10000)
plot(b)
boot.ci(b)

b=boot(pp.incidental$crit_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.incidental$crit_noinput_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

# unaware
b=boot(pp.unaware$crit_input_T1, samplemean, R=10000)
plot(b)
boot.ci(b)

b=boot(pp.unaware$crit_input_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.unaware$crit_input_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.unaware$crit_noinput_T1, samplemean, R=10000)
plot(b)
boot.ci(b)

b=boot(pp.unaware$crit_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.unaware$crit_noinput_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

# Overall (unaware excluded)
b=boot(pp.explicit.incidental$crit_input_T1, samplemean, R=10000)
plot(b)
boot.ci(b)

b=boot(pp.explicit.incidental$crit_input_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit.incidental$crit_input_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit.incidental$crit_noinput_T1, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit.incidental$crit_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit.incidental$crit_noinput_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)


# # control items # #
# explicit
b=boot(pp.explicit$contr_input_T1, samplemean, R=10000)
plot(b)
boot.ci(b)

b=boot(pp.explicit$contr_input_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit$contr_input_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit$contr_noinput_T1, samplemean, R=10000)
plot(b)
boot.ci(b)

b=boot(pp.explicit$contr_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit$contr_noinput_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

# incidental
b=boot(pp.incidental$contr_input_T1, samplemean, R=10000)
plot(b)
boot.ci(b)

b=boot(pp.incidental$contr_input_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.incidental$contr_input_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.incidental$contr_noinput_T1, samplemean, R=10000)
plot(b)
boot.ci(b)

b=boot(pp.incidental$contr_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.incidental$contr_noinput_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

# unaware
b=boot(pp.unaware$contr_input_T1, samplemean, R=10000)
plot(b)
boot.ci(b)

b=boot(pp.unaware$contr_input_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.unaware$contr_input_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.unaware$contr_noinput_T1, samplemean, R=10000)
plot(b)
boot.ci(b)

b=boot(pp.unaware$contr_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.unaware$contr_noinput_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

# Overall (unaware excluded)
b=boot(pp.explicit.incidental$contr_input_T1, samplemean, R=10000)
plot(b)
boot.ci(b)

b=boot(pp.explicit.incidental$contr_input_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit.incidental$contr_input_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit.incidental$contr_noinput_T1, samplemean, R=10000)
plot(b)
boot.ci(b)

b=boot(pp.explicit.incidental$contr_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit.incidental$contr_noinput_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)




## Exploratory data visualization
#################################

# Overview:
# 1) Interacton plots
# 2) Combination of interaction plot and box plot
# 3) Parallel interaction plot 


## 1) Interaction plots ("means plots", "profile plots")
## (They help to understand the interaction, but should not be used to publish the data.)

# Put data into long format

library(reshape2) # load the necessary package
participantsdata.wide <- read.csv("../data/Data_participants_wide.csv", header = TRUE) # load data
colnames(participantsdata.wide)[1] <- "participant" # rename participant variable
is.factor(participantsdata.wide$participant) # check whether Participant is factor (it should be a factor)

# Reduce the dataframe: only keep participant, learningtype and test scores
colnames(participantsdata.wide) # check column numbers
participantsdata.wide <- participantsdata.wide[ ,-c(3:27)] # exclude columns I don't need
colnames(participantsdata.wide) # check again

# Now put into long format
participantsdata.long <- melt(participantsdata.wide, id.vars = c("participant", "learningtype"), variable.name = "variable", value.name = "score") # use melt function to put data into long format
str(participantsdata.long) # tells about structure of the dataframe; check if there are number of pp x 12 observations

# Underscore split: split up the "variable" column in order to get the different factors (verbtype, input, moment)
participantsdata.long <-cbind(participantsdata.long, colsplit(participantsdata.long$variable, "_", names = c("verbtype", "input", "testmoment")))

# The new variables verbtype, input and testmoment still have to be converted to factors
participantsdata.long$verbtype <- factor(participantsdata.long$verbtype); is.factor(participantsdata.long$verbtype)
participantsdata.long$input <- factor(participantsdata.long$input); is.factor(participantsdata.long$input)
participantsdata.long$testmoment <- factor(participantsdata.long$testmoment); is.factor(participantsdata.long$testmoment)


# Create the interaction plots

# Split dataframe for verbtype: control vs. critical (use long format)
criticals <- participantsdata.long [which(participantsdata.long$verbtype == 'crit'), ]
controls <- participantsdata.long [which(participantsdata.long$verbtype == 'contr'), ]

# Create plot for critical items
interaction.plot(criticals$testmoment, criticals$input, criticals$score, type = c("b"), xlab = "Time", ylab = "Mean score", trace.label="Input")

# Create plot for control items
interaction.plot(controls$testmoment, controls$input, controls$score, type = c("b"), xlab = "Time", ylab = "Mean score", trace.label="Input") 

# Do the same for the three learningtype groups separately
pp.explicit.long <- subset(participantsdata.long, subset=learningtype=="explicit") # create subset with explicit learners
pp.incidental.long <- subset(participantsdata.long, subset=learningtype=="incidental") # create subset with incidental learners
pp.unaware.long <- subset(participantsdata.long, subset=learningtype=="unaware") # create subset with unaware participants

# Explicit
criticals <- pp.explicit.long [which(pp.explicit.long$verbtype == 'crit'), ]
controls <- pp.explicit.long [which(pp.explicit.long$verbtype == 'contr'), ]
# Plot critical items
interaction.plot(criticals$testmoment, criticals$input, criticals$score, type = c("b"), xlab = "Time", ylab = "Mean score", trace.label="Input")
# Plot control items
interaction.plot(controls$testmoment, controls$input, controls$score, type = c("b"), xlab = "Time", ylab = "Mean score", trace.label="Input") 

# Incidental
criticals <- pp.incidental.long [which(pp.incidental.long$verbtype == 'crit'), ]
controls <- pp.incidental.long [which(pp.incidental.long$verbtype == 'contr'), ]
# Plot critical items
interaction.plot(criticals$testmoment, criticals$input, criticals$score, type = c("b"), xlab = "Time", ylab = "Mean score", trace.label="Input")
# Plot control items
interaction.plot(controls$testmoment, controls$input, controls$score, type = c("b"), xlab = "Time", ylab = "Mean score", trace.label="Input") 

# Unaware
criticals <- pp.unaware.long [which(pp.unaware.long$verbtype == 'crit'), ]
controls <- pp.unaware.long [which(pp.unaware.long$verbtype == 'contr'), ]
# Plot critical items
interaction.plot(criticals$testmoment, criticals$input, criticals$score, type = c("b"), xlab = "Time", ylab = "Mean score", trace.label="Input")
# Plot control items
interaction.plot(controls$testmoment, controls$input, controls$score, type = c("b"), xlab = "Time", ylab = "Mean score", trace.label="Input") 



## 2) Combination of interaction plot and box plot

library(lattice); library(HH)
attach(participantsdata.long)
interaction2wt(score~learningtype*verbtype*input*testmoment, par.strip.text=list(cex=.6),
               main.in="Effects of Learning Type, Verb Type, Input and Test Moment on Accuracy",
               box.ratio = .3, rot = c(90, 90), factor.expressions =
                 c(Condition=expression("Group"),
                   Type=expression("Verb Type"),
                   Input= expression ("Input"),
                   Moment= expression ("Test Moment")), responselab.expression = "percentage")
# The boxplots can be used to inspect normality (outliers, skew..)



## 3) Parallel interaction plot 

# Source: Larson-Hall 2016, p.422
# Info: Useful plot because it shows individual performance as well as group trends. Requires wide data format.
library(lattice)
colnames(participantsdata.wide)
parallelplot(~participantsdata.wide[c(3, 7)] | participantsdata.wide$learningtype, data = participantsdata.wide) # here: crit_input_T1 vs. crit_input T2
parallelplot(~participantsdata.wide[c(7, 11)] | participantsdata.wide$learningtype, data = participantsdata.wide) # here: crit_input_T2 vs. crit_input T3
parallelplot(~participantsdata.wide[c(8, 12)] | participantsdata.wide$learningtype, data = participantsdata.wide) # here: crit_no_input_T2 vs. crit_no_input T3

# (Remark: It works, but I don't know yet how to handle the different factors in an easier way.)






