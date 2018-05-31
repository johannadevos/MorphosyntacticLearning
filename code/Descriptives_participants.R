# Copyright Eva Koch (2018)

### Script for Study 1 ###
# Created on 8.5.2018
# Aim: Participants description; descriptive statistics for participants and items

rm(list=ls()) # clear workspace
options(contrasts=c("contr.sum","contr.poly")) # Set contrasts to sum-to-zero



######################################################
# Get All BASIC DESCRIPTIVE STATISTICS (participants):
######################################################

# DATA PREPARATION:

# Load data
participantsdata <- read.csv("../data/180529_Data_participants_IV.csv", header = TRUE) # load data
colnames(participantsdata)[1] <- "participant" #rename participant variable
attach(participantsdata)

# Create separate dataframes for different learning types:
pp.explicit <- subset(participantsdata, subset=learningtype=="explicit")
pp.incidental <- subset(participantsdata, subset=learningtype=="incidental")
pp.unaware <- subset(participantsdata, subset=learningtype=="unaware")

# Create a dataframe that only includes explicit and incidental learners
pp.explicit.incidental <- subset(participantsdata, learningtype != "unaware") #drop the "unaware" learners from the dataset
levels(pp.explicit.incidental$learningtype) # although the rows have gone, the factor still has the three levels in it
pp.explicit.incidental <- droplevels(pp.explicit.incidental, pp.explicit.incidental$learningtype=="unaware") # drop the unused factor level
levels(pp.explicit.incidental$learningtype) # OK!

# Separately load data about instruction time
instructiontime <- read.csv("../data/180522_German_instruction.csv", header = TRUE)
colnames(instructiontime)[1] <- "participant" #rename participant variable
instruction.explicit <- subset(instructiontime, subset=Learningtype=="explicit")
instruction.incidental <- subset(instructiontime, subset=Learningtype=="incidental")
instruction.unaware <- subset(instructiontime, subset=Learningtype=="unaware")
levels(instructiontime$Learningtype)

# Trying to drop unused levels, but doesn't currently work
instruction.exp.inc <- subset(instructiontime, Learningtype !="unaware")
levels(instruction.exp.inc$Learningtype)
instruction.exp.inc <- droplevels(instruction.exp.inc, instruction.exp.inc$Learningtype=="unaware")



install.packages("fBasics") # necessary package; attention: only works with numerical data
library(fBasics)

colnames(participantsdata) # to get variable overview (makes the next steps easier)

## INDIVIDUAL VARIABLES ##:

library(data.table) # needed to keep the row names

# lextale (1. create an object with the descriptives; keep the first column with rownames; call the first column 'descriptives')
lextale.all <- basicStats(participantsdata$lextale); setDT(lextale.all, keep.rownames = TRUE); colnames(lextale.all)[1] <- "descriptives"
lextale.exp <- basicStats(pp.explicit$lextale); setDT(lextale.exp, keep.rownames = TRUE); colnames(lextale.exp)[1] <- "descriptives"
lextale.inc <- basicStats(pp.incidental$lextale); setDT(lextale.inc, keep.rownames = TRUE); colnames(lextale.inc)[1] <- "descriptives"
lextale.una <- basicStats(pp.unaware$lextale); setDT(lextale.una, keep.rownames = TRUE); colnames(lextale.una)[1] <- "descriptives"
lextale.all2 <- basicStats(pp.explicit.incidental$lextale); setDT(lextale.all2, keep.rownames = TRUE); colnames(lextale.all2)[1] <- "descriptives"

# age
age.all <- basicStats(participantsdata$age); setDT(age.all, keep.rownames = TRUE); colnames(age.all)[1] <- "descriptives"
age.exp <- basicStats(pp.explicit$age); setDT(age.exp, keep.rownames = TRUE); colnames(age.exp)[1] <- "descriptives"
age.inc <- basicStats(pp.incidental$age); setDT(age.inc, keep.rownames = TRUE); colnames(age.inc)[1] <- "descriptives"
age.una <- basicStats(pp.unaware$age); setDT(age.una, keep.rownames = TRUE); colnames(age.una)[1] <- "descriptives"
age.all2 <- basicStats(pp.explicit.incidental$age); setDT(age.all2, keep.rownames = TRUE); colnames(age.all2)[1] <- "descriptives"

# self-rated proficiency measures:
proficiency.all <- basicStats(participantsdata[, 22:28]); setDT(proficiency.all, keep.rownames = TRUE); colnames(proficiency.all)[1] <- "descriptives"
proficiency.exp <- basicStats(pp.explicit[, 22:28]); setDT(proficiency.exp, keep.rownames = TRUE); colnames(proficiency.exp)[1] <- "descriptives"
proficiency.inc <- basicStats(pp.incidental[, 22:28]); setDT(proficiency.inc, keep.rownames = TRUE); colnames(proficiency.inc)[1] <- "descriptives"
proficiency.una <- basicStats(pp.unaware[, 22:28]); setDT(proficiency.una, keep.rownames = TRUE); colnames(proficiency.una)[1] <- "descriptives"
proficiency.all2 <- basicStats(pp.explicit.incidental[, 22:28]); setDT(proficiency.all2, keep.rownames = TRUE); colnames(proficiency.all2)[1] <- "descriptives"

# Baseline measures: 35-36
baseline.all <- basicStats(participantsdata[, 35:36]); setDT(proficiency.all, keep.rownames = TRUE); colnames(proficiency.all)[1] <- "descriptives"
baseline.exp <- basicStats(pp.explicit[, 35:36]); setDT(proficiency.exp, keep.rownames = TRUE); colnames(proficiency.exp)[1] <- "descriptives"
baseline.inc <- basicStats(pp.incidental[, 35:36]); setDT(proficiency.inc, keep.rownames = TRUE); colnames(proficiency.inc)[1] <- "descriptives"
baseline.una <- basicStats(pp.unaware[, 35:36]); setDT(proficiency.una, keep.rownames = TRUE); colnames(proficiency.una)[1] <- "descriptives"
baseline.all2 <- basicStats(pp.explicit.incidental[, 35:36]); setDT(proficiency.all2, keep.rownames = TRUE); colnames(proficiency.all2)[1] <- "descriptives"

# Number of L2s
L2s_amount.all <- basicStats(participantsdata$L2s_amount); setDT(L2s_amount.all, keep.rownames = TRUE); colnames(L2s_amount.all)[1] <- "descriptives"
L2s_amount.exp <- basicStats(pp.explicit$L2s_amount); setDT(L2s_amount.exp, keep.rownames = TRUE); colnames(L2s_amount.exp)[1] <- "descriptives"
L2s_amount.inc <- basicStats(pp.incidental$L2s_amount); setDT(L2s_amount.inc, keep.rownames = TRUE); colnames(L2s_amount.inc)[1] <- "descriptives"
L2s_amount.una <- basicStats(pp.unaware$L2s_amount); setDT(L2s_amount.una, keep.rownames = TRUE); colnames(L2s_amount.una)[1] <- "descriptives"
L2s_amount.all2 <- basicStats(pp.explicit.incidental$L2s_amount); setDT(L2s_amount.all2, keep.rownames = TRUE); colnames(L2s_amount.all2)[1] <- "descriptives"

# German at school (years; includes school and evening classes; weeks were counted as 0.08; months as 0.25)
School_Evening_years.all <- basicStats(instructiontime$School_Evening_years); setDT(School_Evening_years.all, keep.rownames = TRUE); colnames(School_Evening_years.all)[1] <- "descriptives"
School_Evening_years.exp <- basicStats(instruction.explicit$School_Evening_years); setDT(School_Evening_years.exp, keep.rownames = TRUE); colnames(School_Evening_years.exp)[1] <- "descriptives"
School_Evening_years.inc <- basicStats(instruction.incidental$School_Evening_years); setDT(School_Evening_years.inc, keep.rownames = TRUE); colnames(School_Evening_years.inc)[1] <- "descriptives"
School_Evening_years.una <- basicStats(instruction.unaware$School_Evening_years); setDT(School_Evening_years.una, keep.rownames = TRUE); colnames(School_Evening_years.una)[1] <- "descriptives"
School_Evening_years.all2 <- basicStats(instruction.exp.inc$School_Evening_years); setDT(School_Evening_years.all2, keep.rownames = TRUE); colnames(School_Evening_years.all2)[1] <- "descriptives"

# German at university
German_at_uni_years.all <- basicStats(instructiontime$German_at_uni_years); setDT(German_at_uni_years.all, keep.rownames = TRUE); colnames(German_at_uni_years.all)[1] <- "descriptives"
German_at_uni_years.exp <- basicStats(instruction.explicit$German_at_uni_years); setDT(German_at_uni_years.exp, keep.rownames = TRUE); colnames(German_at_uni_years.exp)[1] <- "descriptives"
German_at_uni_years.inc <- basicStats(instruction.incidental$German_at_uni_years); setDT(German_at_uni_years.inc, keep.rownames = TRUE); colnames(German_at_uni_years.inc)[1] <- "descriptives"
German_at_uni_years.una <- basicStats(instruction.unaware$German_at_uni_years); setDT(German_at_uni_years.una, keep.rownames = TRUE); colnames(German_at_uni_years.una)[1] <- "descriptives"
German_at_uni_years.all2 <- basicStats(instruction.exp.inc$German_at_uni_years); setDT(German_at_uni_years.all2, keep.rownames = TRUE); colnames(German_at_uni_years.all2)[1] <- "descriptives"


## TEST SCORES ##:
scores.all <- basicStats(participantsdata[, 34:77]); setDT(scores.all, keep.rownames = TRUE); colnames(scores.all)[1] <- "descriptives"
scores.exp <- basicStats(pp.explicit[, 34:77]); setDT(scores.exp, keep.rownames = TRUE); colnames(scores.exp)[1] <- "descriptives"
scores.inc <- basicStats(pp.incidental[, 34:77]); setDT(scores.inc, keep.rownames = TRUE); colnames(scores.inc)[1] <- "descriptives"
scores.una <- basicStats(pp.unaware[, 34:77]); setDT(scores.una, keep.rownames = TRUE); colnames(scores.una)[1] <- "descriptives"
scores.all2 <- basicStats(pp.explicit.incidental[, 34:77]); setDT(scores.all2 , keep.rownames = TRUE); colnames(scores.all2 )[1] <- "descriptives"

descriptives.all <- c(lextale.all, age.all, proficiency.all, scores.all, baseline.all, L2s_amount.all, School_Evening_years.all, German_at_uni_years.all) #put all the objects into one list
descriptives.exp <- c(lextale.exp, age.exp, proficiency.exp, scores.exp, baseline.exp, L2s_amount.exp, School_Evening_years.exp, German_at_uni_years.exp) 
descriptives.inc <- c(lextale.inc, age.inc, proficiency.inc, scores.inc, baseline.inc, L2s_amount.inc, School_Evening_years.inc, German_at_uni_years.inc)
descriptives.una <- c(lextale.una, age.una, proficiency.una, scores.una, baseline.una, L2s_amount.una, School_Evening_years.una, German_at_uni_years.una)
descriptives.all2 <- c(lextale.all2, age.all2, proficiency.all2, scores.all2, baseline.all2, L2s_amount.all2, School_Evening_years.all2, German_at_uni_years.all2)

descriptives.output <- c(lextale.all, lextale.exp, lextale.inc, lextale.una, lextale.all2,
                         age.all, age.exp, age.inc, age.una, age.all2,
                         proficiency.all, proficiency.exp, proficiency.inc, proficiency.una, proficiency.all2,
                         baseline.all, baseline.exp, baseline.inc, baseline.una, baseline.all2,
                         scores.all, scores.exp, scores.inc, scores.una, scores.all2) #put everything into one list (but I prefer individual lists)

write.csv(descriptives.output, file = "../results/Descriptives.csv") #put output into 1 csv table
write.csv(descriptives.all, file = "../results/Descriptives_overall.csv") #put output into 1 csv table
write.csv(descriptives.exp, file = "../results/Descriptives_explicit.csv") #put output into 1 csv table
write.csv(descriptives.inc, file = "../results/Descriptives_incidental.csv") #put output into 1 csv table
write.csv(descriptives.una, file = "../results/Descriptives_unaware.csv") #put output into 1 csv table
write.csv(descriptives.all2, file = "../results/Descriptives_overall_exp_inc.csv") #put output into 1 csv table


# Check Confidence Intervals
t.test(pp.explicit$crit_input_T1, conf.level = 0.95)$conf.int # same result as basicStats function :-)


# Get BOOTSTRAPPED CI: (LH, p.97)
# "The boot.ci( ) function takes a bootobject and generates 5 different types of two-sided nonparametric confidence
# intervals. These include the first order normal approximation, the basic bootstrap interval, the studentized bootstrap
# interval, the bootstrap percentile interval, and the adjusted bootstrap percentile (BCa) interval. (internet source)"
install.packages("boot"); library(boot)

samplemean <- function(x, d){
  return(mean(x[d]))
}
# # CRITICAL ITEMS # # 
# explicit
b=boot(pp.explicit$crit_input_T1, samplemean, R=10000)
plot(b) # check if the bootstrap looks like a normal distribution
boot.ci(b) # Output: use BCA!

b=boot(pp.explicit$crit_input_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit$crit_input_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit$crit_noinput_T1, samplemean, R=10000)
plot(b) # check if the bootstrap looks like a normal distribution
boot.ci(b) # Output: use BCA!

b=boot(pp.explicit$crit_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit$crit_noinput_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)


# incidental
b=boot(pp.incidental$crit_input_T1, samplemean, R=10000)
plot(b) # check if the bootstrap looks like a normal distribution
boot.ci(b) # Output: use BCA!

b=boot(pp.incidental$crit_input_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.incidental$crit_input_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.incidental$crit_noinput_T1, samplemean, R=10000)
plot(b) # check if the bootstrap looks like a normal distribution
boot.ci(b) # Output: use BCA!

b=boot(pp.incidental$crit_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.incidental$crit_noinput_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)


# unaware
b=boot(pp.unaware$crit_input_T1, samplemean, R=10000)
plot(b) # check if the bootstrap looks like a normal distribution
boot.ci(b) # Output: use BCA!

b=boot(pp.unaware$crit_input_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.unaware$crit_input_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.unaware$crit_noinput_T1, samplemean, R=10000)
plot(b) # check if the bootstrap looks like a normal distribution
boot.ci(b) # Output: use BCA!

b=boot(pp.unaware$crit_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.unaware$crit_noinput_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)


# Overall
b=boot(participantsdata$crit_input_T1, samplemean, R=10000)
plot(b)
boot.ci(b)

b=boot(participantsdata$crit_input_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(participantsdata$crit_input_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(participantsdata$crit_noinput_T1, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(participantsdata$crit_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(participantsdata$crit_noinput_T3, samplemean, R=10000)
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


# # CONTROL ITEMS # #
# explicit
b=boot(pp.explicit$contr_input_T1, samplemean, R=10000)
plot(b) # check if the bootstrap looks like a normal distribution
boot.ci(b) # Output: use BCA!

b=boot(pp.explicit$contr_input_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit$contr_input_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit$contr_noinput_T1, samplemean, R=10000)
plot(b) # check if the bootstrap looks like a normal distribution
boot.ci(b) # Output: use BCA!

b=boot(pp.explicit$contr_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit$contr_noinput_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)


# incidental
b=boot(pp.incidental$contr_input_T1, samplemean, R=10000)
plot(b) # check if the bootstrap looks like a normal distribution
boot.ci(b) # Output: use BCA!

b=boot(pp.incidental$contr_input_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.incidental$contr_input_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.incidental$contr_noinput_T1, samplemean, R=10000)
plot(b) # check if the bootstrap looks like a normal distribution
boot.ci(b) # Output: use BCA!

b=boot(pp.incidental$contr_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.incidental$contr_noinput_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)


# unaware
b=boot(pp.unaware$contr_input_T1, samplemean, R=10000)
plot(b) # check if the bootstrap looks like a normal distribution
boot.ci(b) # Output: use BCA!

b=boot(pp.unaware$contr_input_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.unaware$contr_input_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.unaware$contr_noinput_T1, samplemean, R=10000)
plot(b) # check if the bootstrap looks like a normal distribution
boot.ci(b) # Output: use BCA!

b=boot(pp.unaware$contr_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.unaware$contr_noinput_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)


# Overall
b=boot(participantsdata$contr_input_T1, samplemean, R=10000)
plot(b) # check if the bootstrap looks like a normal distribution
boot.ci(b) # Output: use BCA!

b=boot(participantsdata$contr_input_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(participantsdata$contr_input_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(participantsdata$contr_noinput_T1, samplemean, R=10000)
plot(b) # check if the bootstrap looks like a normal distribution
boot.ci(b) # Output: use BCA!

b=boot(participantsdata$contr_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(participantsdata$contr_noinput_T3, samplemean, R=10000)
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
plot(b) # check if the bootstrap looks like a normal distribution
boot.ci(b) # Output: use BCA!

b=boot(pp.explicit.incidental$contr_noinput_T2, samplemean, R=10000)
plot(b) 
boot.ci(b)

b=boot(pp.explicit.incidental$contr_noinput_T3, samplemean, R=10000)
plot(b) 
boot.ci(b)



##################################
# EXPLORATORY DATA VISUALISATION:
##################################


### MAKE INTERACTION PLOTS ("means plots", "profile plots") ###
###############################################################

# PUT DATA INTO LONG FORMAT
participantsdata.wide <- read.csv("../data/180529_Data_participants_IV.csv", header = TRUE) # load data
colnames(participantsdata.wide)[1] <- "participant" #rename participant variable

install.packages("reshape2") # install reshape package to put datafile from wide into long format
library(reshape2) # load the necessary package

attach(participantsdata.wide) # attach dataframe
is.factor(participant) # check whether Participant is factor (it should be a factor)

# reduce the dataframe: only keep participant, learningtype and test scores
colnames(participantsdata.wide) #check column numbers
participantsdata.wide <- participantsdata.wide[, -c(2, 4:36, 49:77)] #exclude columns I don't need
colnames(participantsdata.wide) #check

# put into long format:
participantsdata.long <- melt(participantsdata.wide, id.vars = c("participant", "learningtype"), variable.name = "variable", value.name = "score") # use melt function to put data into long format
str(participantsdata.long) # tells about structure of the dataframe; check if there are number of pp x 12 observations

# underscore split: split up the "variable" column in order to get the different factors (verbtype, input, moment)
participantsdata.long <-cbind(participantsdata.long, colsplit(participantsdata.long$variable, "_", names = c("verbtype", "input", "moment")))

# Type, Input and Moment still have to be converted to factors (necessary for ANOVA)
participantsdata.long$verbtype <- factor(participantsdata.long$verbtype); is.factor(participantsdata.long$verbtype)
participantsdata.long$input <- factor(participantsdata.long$input); is.factor(participantsdata.long$input)
participantsdata.long$moment <- factor(participantsdata.long$moment); is.factor(participantsdata.long$moment)


# CREATE INTERACTION PLOTS
# --> to help understand the interactions, but not to publish the data

# split dataframe for Type: control vs critical (use long format)
criticals <- participantsdata.long [which(participantsdata.long$verbtype == 'crit'), ]
controls <- participantsdata.long [which(participantsdata.long$verbtype == 'contr'), ]

# plot for critical items:
interaction.plot(criticals$moment, criticals$input, criticals$score,
                 type = c("b"), xlab = "Time", ylab = "Mean score", trace.label="Input")

# plot for control items:
interaction.plot(controls$moment, controls$input, controls$score,
                 type = c("b"), xlab = "Time", ylab = "Mean score", trace.label="Input") 

# do the same for explicit / incidental / unaware groups separately:
pp.explicit.long <- subset(participantsdata.long, subset=learningtype=="explicit") # create subset with explicit learners
pp.incidental.long <- subset(participantsdata.long, subset=learningtype=="incidental") # create subset with incidental learners
pp.unaware.long <- subset(participantsdata.long, subset=learningtype=="unaware") # create subset with unaware participants

# explicit:
criticals <- pp.explicit.long [which(pp.explicit.long$verbtype == 'crit'), ]
controls <- pp.explicit.long [which(pp.explicit.long$verbtype == 'contr'), ]
# plot critical items:
interaction.plot(criticals$moment, criticals$input, criticals$score, type = c("b"), xlab = "Time", ylab = "Mean score", trace.label="Input")
# plot for control items:
interaction.plot(controls$moment, controls$input, controls$score, type = c("b"), xlab = "Time", ylab = "Mean score", trace.label="Input") 

# incidental:
criticals <- pp.incidental.long [which(pp.incidental.long$verbtype == 'crit'), ]
controls <- pp.incidental.long [which(pp.incidental.long$verbtype == 'contr'), ]
# plot critical items:
interaction.plot(criticals$moment, criticals$input, criticals$score, type = c("b"), xlab = "Time", ylab = "Mean score", trace.label="Input")
# plot for control items:
interaction.plot(controls$moment, controls$input, controls$score, type = c("b"), xlab = "Time", ylab = "Mean score", trace.label="Input") 

# unaware:
criticals <- pp.unaware.long [which(pp.unaware.long$verbtype == 'crit'), ]
controls <- pp.unaware.long [which(pp.unaware.long$verbtype == 'contr'), ]
# plot critical items:
interaction.plot(criticals$moment, criticals$input, criticals$score, type = c("b"), xlab = "Time", ylab = "Mean score", trace.label="Input")
# plot for control items:
interaction.plot(controls$moment, controls$input, controls$score, type = c("b"), xlab = "Time", ylab = "Mean score", trace.label="Input") 



##  COMBINATION INTERACTION PLOT AND BOX PLOT (Larson-Hall) ##
##############################################################

attach(participantsdata.long)
install.packages("HH")
library(lattice)
library(HH)
interaction2wt(score~learningtype*verbtype*input*moment, par.strip.text=list(cex=.6),
               main.in="Effects of Learning Type, Verb Type, Input and Test Moment on Accuracy",
               box.ratio = .3, rot = c(90, 90), factor.expressions =
                 c(Condition=expression("Group"),
                   Type=expression("Verb Type"),
                   Input= expression ("Input"),
                   Moment= expression ("Test Moment")), responselab.expression = "bla")
# the boxplots can be used to inspect normality (outliers, skew..)



## PARALLEL INTERACTION PLOT (LH, p.422) ##
###########################################

# good because shows individual performance as well as group trends. wide format needed!
library(lattice)
parallelplot(~participantsdata.wide[c(3, 7)] | participantsdata.wide$learningtype, data = participantsdata.wide)
# here: crit_input_T1 vs. crit_input T2
# works, but I don't know yet how to handle the different factors in an easier way