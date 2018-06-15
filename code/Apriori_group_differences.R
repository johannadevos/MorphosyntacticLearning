# Copyright Eva Koch (2018)

###########################################
# INVESTIGATE A PRIORI GROUP DIFFERENCES: #
###########################################

# Script written in May/June 2018 to analyse the Study 1 data. More specifically to check if there
# were any a priori group differences between the explicit and the incidental groups.

# set working directory to source file location

rm(list=ls()) # clear workspace

options(contrasts=c("contr.sum","contr.poly")) # Set contrasts to sum-to-zero


# Prepare datasets:

# Load data
participantsdata <- read.csv("../data/Data_participants_wide.csv", header = TRUE) # load data
colnames(participantsdata)[1] <- "participant" #rename participant variable
str(participantsdata)

# Turn self-rated variables into ordered factor:
# (Info: Scientists often treat likert scale data as interval data, but this practice is questionable. We decided to treat
# our 5 point likert scale data as ordinal. Oridnal = categorical, when the categories are ordered. This is also recommended
# by Field, Miles & Field, 2012, p.9; Jane Superbrain box 1.2)
proficiency_overall <- factor(participantsdata$proficiency_overall, ordered=TRUE)
proficiency_written <- factor(participantsdata$proficiency_written, ordered=TRUE)
proficiency_oral <- factor(participantsdata$proficiency_oral, ordered=TRUE)
comprehension_oral <- factor(participantsdata$comprehension_oral, ordered=TRUE)
comprehension_written <- factor(participantsdata$comprehension_written, ordered=TRUE)
usage <- factor(participantsdata$usage, ordered=TRUE)

# Create subsets (per learningtype):
pp.explicit <- subset(participantsdata, subset=learningtype=="explicit")
pp.incidental <- subset(participantsdata, subset=learningtype=="incidental")
pp.unaware <- subset(participantsdata, subset=learningtype=="unaware")

# Create a dataset for explicit/incidental groups only (exclude unaware)
# (Info: some tests only work if the grouping factor has 2 levels.)
pp.explicit.incidental <- subset(participantsdata, learningtype != "unaware") #drop the "unaware" learners from the dataset
levels(pp.explicit.incidental$learningtype) # although the rows have gone, the factor still has the three levels in it
pp.explicit.incidental$learningtype <- factor(pp.explicit.incidental$learningtype) # turn the same factor into a factor again
levels(pp.explicit.incidental$learningtype) # the factor has now only two levels.


# Variables for which we need to check (ideally, rule out) group differences

# Info: the grouping variable is learningtype; 'explicit' vs. 'incidental'.
# (We are less interested in 'unaware' because it's a very small group that won't be included in the main analysis.)

colnames(participantsdata)

# Levels of measurement:

# Continuous variables: ratio-scaled
#   lextale
#   age [DISCRETE]
#   PRE_CRIT
#   PRE_CONTR
#   L2s_amount (=amount of L2s) ? [DISCRETE]
#   proficiency_average (# Info: Proficiency average covers overall (50%), writing (25%), listening (25%), speaking (25%), reading (25%); we treat is as ratio-scaled and
#   not as ordinal since it is calculated as an average of multiple ratings)
# Instruction-time variables:
#   school_evening_years (=years of instruction at school & evening classes) [DISCRETE]
#   german_at_uni_years (=years of German at university-level) [DISCRETE]

# Categorical (binary):
#   bilingualism (=bilingual/monolingual)
#   L11 (= NL-BE: Flemish/NL-NL: Dutch from the Netherlands)

# Ordinal:
# Self-ratings (5 point likert):
#   proficiency_overall
#   proficiency_oral
#   proficiency_written
#   comprehension_oral
#   comprehension_written
#   usage


# 1) DESCRIPTIVE STATISTICS
###########################

# Info: we will ask for full descriptive statistics for the individual differences variables; we will do this for the full
# dataset ("all"), as well as for the three learningtype groups ("exp", "inc", "una"). "all2" refers to descriptives across
# the explicit and incidental, but not the unaware groups.

install.packages("fBasics"); library(fBasics) # necessary package; attention: only works with numerical data
library(data.table) # needed to keep the row names

colnames(participantsdata)

# meaning of the formula: create an object with the descriptives; keep the first column with rownames; call the first column 'descriptives'

# lextale
lextale.all <- basicStats(participantsdata$lextale); setDT(lextale.all, keep.rownames = TRUE); colnames(lextale.all)[1] <- "descriptives"; lextale.all
lextale.exp <- basicStats(pp.explicit$lextale); setDT(lextale.exp, keep.rownames = TRUE); colnames(lextale.exp)[1] <- "descriptives"; lextale.exp
lextale.inc <- basicStats(pp.incidental$lextale); setDT(lextale.inc, keep.rownames = TRUE); colnames(lextale.inc)[1] <- "descriptives"; lextale.inc
lextale.una <- basicStats(pp.unaware$lextale); setDT(lextale.una, keep.rownames = TRUE); colnames(lextale.una)[1] <- "descriptives"; lextale.una
lextale.all2 <- basicStats(pp.explicit.incidental$lextale); setDT(lextale.all2, keep.rownames = TRUE); colnames(lextale.all2)[1] <- "descriptives"; lextale.all2

# age
age.all <- basicStats(participantsdata$age); setDT(age.all, keep.rownames = TRUE); colnames(age.all)[1] <- "descriptives"; age.all
age.exp <- basicStats(pp.explicit$age); setDT(age.exp, keep.rownames = TRUE); colnames(age.exp)[1] <- "descriptives"; age.exp
age.inc <- basicStats(pp.incidental$age); setDT(age.inc, keep.rownames = TRUE); colnames(age.inc)[1] <- "descriptives"; age.inc
age.una <- basicStats(pp.unaware$age); setDT(age.una, keep.rownames = TRUE); colnames(age.una)[1] <- "descriptives"; age.una
age.all2 <- basicStats(pp.explicit.incidental$age); setDT(age.all2, keep.rownames = TRUE); colnames(age.all2)[1] <- "descriptives"; age.all2

# self-rated proficiency measures: columns 10-16
selfratings.all <- basicStats(participantsdata[, 10:16]); setDT(selfratings.all, keep.rownames = TRUE); colnames(selfratings.all)[1] <- "descriptives"; selfratings.all
selfratings.exp <- basicStats(pp.explicit[, 10:16]); setDT(selfratings.exp, keep.rownames = TRUE); colnames(selfratings.exp)[1] <- "descriptives"; selfratings.exp
selfratings.inc <- basicStats(pp.incidental[, 10:16]); setDT(selfratings.inc, keep.rownames = TRUE); colnames(selfratings.inc)[1] <- "descriptives"; selfratings.inc
selfratings.una <- basicStats(pp.unaware[, 10:16]); setDT(selfratings.una, keep.rownames = TRUE); colnames(selfratings.una)[1] <- "descriptives"; selfratings.una
selfratings.all2 <- basicStats(pp.explicit.incidental[, 10:16]); setDT(selfratings.all2, keep.rownames = TRUE); colnames(selfratings.all2)[1] <- "descriptives"; selfratings.all2

# pre-measures: columns 26-27
baseline.all <- basicStats(participantsdata[, 26:27]); setDT(baseline.all, keep.rownames = TRUE); colnames(baseline.all)[1] <- "descriptives"; baseline.all
baseline.exp <- basicStats(pp.explicit[, 26:27]); setDT(baseline.exp, keep.rownames = TRUE); colnames(baseline.exp)[1] <- "descriptives"; baseline.exp
baseline.inc <- basicStats(pp.incidental[, 26:27]); setDT(baseline.inc, keep.rownames = TRUE); colnames(baseline.inc)[1] <- "descriptives"; baseline.inc
baseline.una <- basicStats(pp.unaware[, 26:27]); setDT(baseline.una, keep.rownames = TRUE); colnames(baseline.una)[1] <- "descriptives"; baseline.una
baseline.all2 <- basicStats(pp.explicit.incidental[, 26:27]); setDT(baseline.all2, keep.rownames = TRUE); colnames(baseline.all2)[1] <- "descriptives"; baseline.all2

# Number of L2s
L2s_amount.all <- basicStats(participantsdata$L2s_amount); setDT(L2s_amount.all, keep.rownames = TRUE); colnames(L2s_amount.all)[1] <- "descriptives"; L2s_amount.all
L2s_amount.exp <- basicStats(pp.explicit$L2s_amount); setDT(L2s_amount.exp, keep.rownames = TRUE); colnames(L2s_amount.exp)[1] <- "descriptives"; L2s_amount.exp
L2s_amount.inc <- basicStats(pp.incidental$L2s_amount); setDT(L2s_amount.inc, keep.rownames = TRUE); colnames(L2s_amount.inc)[1] <- "descriptives"; L2s_amount.inc
L2s_amount.una <- basicStats(pp.unaware$L2s_amount); setDT(L2s_amount.una, keep.rownames = TRUE); colnames(L2s_amount.una)[1] <- "descriptives"; L2s_amount.una
L2s_amount.all2 <- basicStats(pp.explicit.incidental$L2s_amount); setDT(L2s_amount.all2, keep.rownames = TRUE); colnames(L2s_amount.all2)[1] <- "descriptives"; L2s_amount.all2

# instructiontime in years: columns 24-25 
instructiontime.all <- basicStats(participantsdata[, 24:25]); setDT(instructiontime.all, keep.rownames = TRUE); colnames(instructiontime.all)[1] <- "descriptives"; instructiontime.all
instructiontime.exp <- basicStats(pp.explicit[, 24:25]); setDT(instructiontime.exp, keep.rownames = TRUE); colnames(instructiontime.exp)[1] <- "descriptives"; instructiontime.exp
instructiontime.inc <- basicStats(pp.incidental[, 24:25]); setDT(instructiontime.inc, keep.rownames = TRUE); colnames(instructiontime.inc)[1] <- "descriptives"; instructiontime.inc
instructiontime.una <- basicStats(pp.unaware[, 24:25]); setDT(instructiontime.una, keep.rownames = TRUE); colnames(instructiontime.una)[1] <- "descriptives"; instructiontime.una
instructiontime.all2 <- basicStats(pp.explicit.incidental[, 24:25]); setDT(instructiontime.all2, keep.rownames = TRUE); colnames(instructiontime.all2)[1] <- "descriptives"; instructiontime.all2


# combine output
descriptives.all <- c(lextale.all, age.all, selfratings.all, baseline.all, L2s_amount.all, instructiontime.all)
descriptives.exp <- c(lextale.exp, age.exp, selfratings.exp, baseline.exp, L2s_amount.exp, instructiontime.exp)
descriptives.inc <- c(lextale.inc, age.inc, selfratings.inc, baseline.inc, L2s_amount.inc, instructiontime.inc)
descriptives.una <- c(lextale.una, age.una, selfratings.una, baseline.una, L2s_amount.una, instructiontime.una)
descriptives.all2 <- c(lextale.all2, age.all2, selfratings.all2, baseline.all2, L2s_amount.all2, instructiontime.all2)

# write csv output
write.csv(descriptives.all, file = "../results/Descriptives_overall.csv") 
write.csv(descriptives.exp, file = "../results/Descriptives_explicit.csv") 
write.csv(descriptives.inc, file = "../results/Descriptives_incidental.csv") 
write.csv(descriptives.una, file = "../results/Descriptives_unaware.csv") 
write.csv(descriptives.all2, file = "../results/Descriptives_overall_exp_inc.csv")


# Get medians and interquartile ranges
attach(participantsdata)

tapply(age, learningtype, median)
tapply(age, learningtype, IQR)

tapply(school_evening_years, learningtype, median)
tapply(school_evening_years, learningtype, IQR)

tapply(german_at_uni_years, learningtype, median)
tapply(german_at_uni_years, learningtype, IQR)

tapply(L2s_amount, learningtype, median)
tapply(L2s_amount, learningtype, IQR)

tapply(PRE_CONTR, learningtype, median)
tapply(PRE_CONTR, learningtype, IQR)

tapply(PRE_CRIT, learningtype, median)
tapply(PRE_CRIT, learningtype, IQR)

tapply(usage, learningtype, median)
tapply(usage, learningtype, IQR)

tapply(proficiency_overall, learningtype, median)
tapply(proficiency_overall, learningtype, IQR)

tapply(proficiency_oral, learningtype, median)
tapply(proficiency_oral, learningtype, IQR)

tapply(proficiency_written, learningtype, median)
tapply(proficiency_written, learningtype, IQR)

tapply(comprehension_oral, learningtype, median)
tapply(comprehension_oral, learningtype, IQR)

tapply(comprehension_written, learningtype, median)
tapply(comprehension_written, learningtype, IQR)

detach(participantsdata)


# 2) CHECK ASSUMPTION OF NORMALITY
##################################

# Normality diagnostics (check per variable and per group):
# a) Shapiro-Wilk test
# b) Skew and kurtosis values
# c) Visual data inspection (QQ-Plot, Boxplots, Histogram with normality line)


# a) Shapiro-Wilk test (should not be significant)

# lextale
shapiro.lextale.exp <- shapiro.test(pp.explicit$lextale); shapiro.lextale.exp
shapiro.lextale.inc <- shapiro.test(pp.incidental$lextale); shapiro.lextale.inc
shapiro.lextale.una <- shapiro.test(pp.unaware$lextale); shapiro.lextale.una
# proficiency_overall
shapiro.proficiency_overall.exp <- shapiro.test(pp.explicit$proficiency_overall)
shapiro.proficiency_overall.inc <- shapiro.test(pp.incidental$proficiency_overall)
shapiro.proficiency_overall.una <- shapiro.test(pp.unaware$proficiency_overall)
# proficiency_oral
shapiro.proficiency_oral.exp <- shapiro.test(pp.explicit$proficiency_oral)
shapiro.proficiency_oral.inc <- shapiro.test(pp.incidental$proficiency_oral)
shapiro.proficiency_oral.una <- shapiro.test(pp.unaware$proficiency_oral)
# proficiency_written
shapiro.proficiency_written.exp <- shapiro.test(pp.explicit$proficiency_written)
shapiro.proficiency_written.inc <- shapiro.test(pp.incidental$proficiency_written)
shapiro.proficiency_written.una <- shapiro.test(pp.unaware$proficiency_written)
# proficiency_average
shapiro.proficiency_average.exp <- shapiro.test(pp.explicit$proficiency_average); shapiro.proficiency_average.exp
shapiro.proficiency_average.inc <- shapiro.test(pp.incidental$proficiency_average); shapiro.proficiency_average.inc
shapiro.proficiency_average.una <- shapiro.test(pp.unaware$proficiency_average)
# comprehension_oral
shapiro.comprehension_oral.exp <- shapiro.test(pp.explicit$comprehension_oral)
shapiro.comprehension_oral.inc <- shapiro.test(pp.incidental$comprehension_oral)
shapiro.comprehension_oral.una <- shapiro.test(pp.unaware$comprehension_oral)
# comprehension_written
shapiro.comprehension_written.exp <- shapiro.test(pp.explicit$comprehension_written)
shapiro.comprehension_written.inc <- shapiro.test(pp.incidental$comprehension_written)
shapiro.comprehension_written.una <- shapiro.test(pp.unaware$comprehension_written)
# PRE_CRIT
shapiro.PRE_CRIT.exp <- shapiro.test(pp.explicit$PRE_CRIT); shapiro.PRE_CRIT.exp
shapiro.PRE_CRIT.inc <- shapiro.test(pp.incidental$PRE_CRIT); shapiro.PRE_CRIT.inc
shapiro.PRE_CRIT.una <- shapiro.test(pp.unaware$PRE_CRIT) # error (I'll exclude it from the list)
# PRE_CONTR
shapiro.PRE_CONTR.exp <- shapiro.test(pp.explicit$PRE_CONTR); shapiro.PRE_CONTR.exp
shapiro.PRE_CONTR.inc <- shapiro.test(pp.incidental$PRE_CONTR); shapiro.PRE_CONTR.inc
shapiro.PRE_CONTR.una <- shapiro.test(pp.unaware$PRE_CONTR)
# usage
shapiro.usage.exp <- shapiro.test(pp.explicit$usage)
shapiro.usage.inc <- shapiro.test(pp.incidental$usage)
shapiro.usage.una <- shapiro.test(pp.unaware$usage)
# age
shapiro.age.exp <- shapiro.test(pp.explicit$age); shapiro.age.exp
shapiro.age.inc <- shapiro.test(pp.incidental$age); shapiro.age.inc
shapiro.age.una <- shapiro.test(pp.unaware$age)
# school_evening_years
shapiro.school.exp <- shapiro.test(pp.explicit$school_evening_years); shapiro.school.exp
shapiro.school.inc <- shapiro.test(pp.incidental$school_evening_years); shapiro.school.inc
shapiro.school.una <- shapiro.test(pp.unaware$school_evening_years)
# german_at_uni_years
shapiro.uni.exp <- shapiro.test(pp.explicit$german_at_uni_years); shapiro.uni.exp
shapiro.uni.inc <- shapiro.test(pp.incidental$german_at_uni_years); shapiro.uni.inc
shapiro.uni.una <- shapiro.test(pp.unaware$german_at_uni_years)
# amount of L2s (L2s_amount)
shapiro.L2s.exp <- shapiro.test(pp.explicit$L2s_amount); shapiro.L2s.exp
shapiro.L2s.inc <- shapiro.test(pp.incidental$L2s_amount); shapiro.L2s.inc
shapiro.L2s.una <- shapiro.test(pp.unaware$L2s_amount)

# bind all results together
shapiro.results <- cbind(shapiro.lextale.exp, shapiro.lextale.inc, shapiro.lextale.una,
                         shapiro.proficiency_overall.exp, shapiro.proficiency_overall.inc, shapiro.proficiency_overall.una, 
                         shapiro.proficiency_oral.exp, shapiro.proficiency_oral.inc, shapiro.proficiency_oral.una,
                         shapiro.proficiency_written.exp, shapiro.proficiency_written.inc, shapiro.proficiency_written.una,
                         shapiro.proficiency_average.exp, shapiro.proficiency_average.inc, shapiro.proficiency_average.una,
                         shapiro.comprehension_oral.exp, shapiro.comprehension_oral.inc, shapiro.comprehension_oral.una,
                         shapiro.comprehension_written.exp, shapiro.comprehension_written.inc, shapiro.comprehension_written.una,
                         shapiro.PRE_CRIT.exp, shapiro.PRE_CRIT.inc, 
                         shapiro.PRE_CONTR.exp, shapiro.PRE_CONTR.inc, shapiro.PRE_CONTR.una,
                         shapiro.usage.exp, shapiro.usage.inc, shapiro.usage.una,
                         shapiro.age.exp, shapiro.age.inc, shapiro.age.una,
                         shapiro.school.exp, shapiro.age.inc, shapiro.age.una,
                         shapiro.uni.exp, shapiro.uni.inc, shapiro.uni.una,
                         shapiro.L2s.exp, shapiro.L2s.inc, shapiro.L2s.una)

write.csv(shapiro.results, file="../results/Shapiro.csv") # write csv output


# b) Check skew and kurtosis (skew.2SE should be smaller than 1)

library(pastecs)
# lextale
desc.lextale.exp <- stat.desc(pp.explicit$lextale, basic=TRUE, norm=TRUE); desc.lextale.exp
desc.lextale.inc <- stat.desc(pp.incidental$lextale, basic=TRUE, norm=TRUE); desc.lextale.inc
desc.lextale.una <- stat.desc(pp.unaware$lextale, basic=TRUE, norm=TRUE); desc.lextale.una
# proficiency_overall
desc.proficiency_overall.exp <- stat.desc(pp.explicit$proficiency_overall, basic=TRUE, norm=TRUE)
desc.proficiency_overall.inc <- stat.desc(pp.incidental$proficiency_overall, basic=TRUE, norm=TRUE)
desc.proficiency_overall.una <- stat.desc(pp.unaware$proficiency_overall, basic=TRUE, norm=TRUE)
# proficiency_oral
desc.proficiency_oral.exp <- stat.desc(pp.explicit$proficiency_oral, basic=TRUE, norm=TRUE)
desc.proficiency_oral.inc <- stat.desc(pp.incidental$proficiency_oral, basic=TRUE, norm=TRUE)
desc.proficiency_oral.una <- stat.desc(pp.unaware$proficiency_oral, basic=TRUE, norm=TRUE)
# proficiency_written
desc.proficiency_written.exp <- stat.desc(pp.explicit$proficiency_written, basic=TRUE, norm=TRUE)
desc.proficiency_written.inc <- stat.desc(pp.incidental$proficiency_written, basic=TRUE, norm=TRUE)
desc.proficiency_written.una <- stat.desc(pp.unaware$proficiency_written, basic=TRUE, norm=TRUE)
# proficiency_average
desc.proficiency_average.exp <- stat.desc(pp.explicit$proficiency_average, basic=TRUE, norm=TRUE); desc.proficiency_average.exp
desc.proficiency_average.inc <- stat.desc(pp.incidental$proficiency_average, basic=TRUE, norm=TRUE); desc.proficiency_average.inc
desc.proficiency_average.una <- stat.desc(pp.unaware$proficiency_average, basic=TRUE, norm=TRUE)
# comprehension_oral
desc.comprehension_oral.exp <- stat.desc(pp.explicit$comprehension_oral, basic=TRUE, norm=TRUE)
desc.comprehension_oral.inc <- stat.desc(pp.incidental$comprehension_oral, basic=TRUE, norm=TRUE)
desc.comprehension_oral.una <- stat.desc(pp.unaware$comprehension_oral, basic=TRUE, norm=TRUE)
# comprehension_written
desc.comprehension_written.exp <- stat.desc(pp.explicit$comprehension_written, basic=TRUE, norm=TRUE)
desc.comprehension_written.inc <- stat.desc(pp.incidental$comprehension_written, basic=TRUE, norm=TRUE)
desc.comprehension_written.una <- stat.desc(pp.unaware$comprehension_written, basic=TRUE, norm=TRUE)
# PRE_CRIT
desc.PRE_CRIT.exp <- stat.desc(pp.explicit$PRE_CRIT, basic=TRUE, norm=TRUE); desc.PRE_CRIT.exp
desc.PRE_CRIT.inc <- stat.desc(pp.incidental$PRE_CRIT, basic=TRUE, norm=TRUE); desc.PRE_CRIT.inc
desc.PRE_CRIT.una <- stat.desc(pp.unaware$PRE_CRIT, basic=TRUE, norm=TRUE) # error (I'll exclude it from the list)
# PRE_CONTR
desc.PRE_CONTR.exp <- stat.desc(pp.explicit$PRE_CONTR, basic=TRUE, norm=TRUE); desc.PRE_CONTR.exp
desc.PRE_CONTR.inc <- stat.desc(pp.incidental$PRE_CONTR, basic=TRUE, norm=TRUE); desc.PRE_CONTR.inc
desc.PRE_CONTR.una <- stat.desc(pp.unaware$PRE_CONTR, basic=TRUE, norm=TRUE)
# usage
desc.usage.exp <- stat.desc(pp.explicit$usage, basic=TRUE, norm=TRUE)
desc.usage.inc <- stat.desc(pp.incidental$usage, basic=TRUE, norm=TRUE)
desc.usage.una <- stat.desc(pp.unaware$usage, basic=TRUE, norm=TRUE)
# age
desc.age.exp <- stat.desc(pp.explicit$age, basic=TRUE, norm=TRUE); desc.age.exp
desc.age.inc <- stat.desc(pp.incidental$age, basic=TRUE, norm=TRUE); desc.age.inc
desc.age.una <- stat.desc(pp.unaware$age, basic=TRUE, norm=TRUE)
# school_evening_years
desc.school.exp <- stat.desc(pp.explicit$school_evening_years, basic=TRUE, norm=TRUE); desc.school.exp
desc.school.inc <- stat.desc(pp.incidental$school_evening_years, basic=TRUE, norm=TRUE); desc.school.inc
desc.school.una <- stat.desc(pp.unaware$school_evening_years, basic=TRUE, norm=TRUE)
# german_at_uni_years
desc.uni.exp <- stat.desc(pp.explicit$german_at_uni_years, basic=TRUE, norm=TRUE); desc.uni.exp
desc.uni.inc <- stat.desc(pp.incidental$german_at_uni_years, basic=TRUE, norm=TRUE); desc.uni.inc
desc.uni.una <- stat.desc(pp.unaware$german_at_uni_years, basic=TRUE, norm=TRUE)
# amount of L2s (L2s_amount)
desc.L2s.exp <- stat.desc(pp.explicit$L2s_amount, basic=TRUE, norm=TRUE); desc.L2s.exp
desc.L2s.inc <- stat.desc(pp.incidental$L2s_amount, basic=TRUE, norm=TRUE); desc.L2s.inc
desc.L2s.una <- stat.desc(pp.unaware$L2s_amount, basic=TRUE, norm=TRUE)

# bind results together
desc.results <- cbind(desc.lextale.exp,desc.lextale.inc,desc.lextale.una,
                      desc.proficiency_overall.exp,desc.proficiency_overall.inc,desc.proficiency_overall.una,
                      desc.proficiency_oral.exp,desc.proficiency_oral.inc,desc.proficiency_oral.una,
                      desc.proficiency_written.exp,desc.proficiency_written.inc,desc.proficiency_written.una,
                      desc.proficiency_average.exp,desc.proficiency_average.inc,desc.proficiency_average.una,
                      desc.comprehension_oral.exp,desc.comprehension_oral.inc,desc.comprehension_oral.una,
                      desc.comprehension_written.exp,desc.comprehension_written.inc,desc.comprehension_written.una,
                      desc.PRE_CRIT.exp,desc.PRE_CRIT.inc, desc.PRE_CONTR.exp,desc.PRE_CONTR.inc,desc.PRE_CONTR.una,
                      desc.usage.exp, desc.usage.inc, desc.usage.una,
                      desc.age.exp, desc.age.inc, desc.age.una,
                      desc.school.exp, desc.school.inc, desc.school.una,
                      desc.uni.exp, desc.uni.inc, desc.uni.una,
                      desc.L2s.exp, desc.L2s.inc, desc.L2s.una)

write.csv(desc.results, file="../results/Skew_and_kurtosis.csv") # write csv output


# c) QQ-plots (dots should be on a straight line)

library(ggplot2)
# lextale
qqplot.lextale.exp <- qplot(sample=pp.explicit$lextale, stat="qq"); qqplot.lextale.exp
qqplot.lextale.inc <- qplot(sample=pp.incidental$lextale, stat="qq"); qqplot.lextale.inc
qqplot.lextale.una <- qplot(sample=pp.unaware$lextale, stat="qq"); qqplot.lextale.una
# proficiency_overall
qqplot.proficiency_overall.exp <- qplot(sample=pp.explicit$proficiency_overall, stat="qq"); qqplot.proficiency_overall.exp
qqplot.proficiency_overall.inc <- qplot(sample=pp.incidental$proficiency_overall, stat="qq"); qqplot.proficiency_overall.inc
qqplot.proficiency_overall.una <- qplot(sample=pp.unaware$proficiency_overall, stat="qq"); qqplot.proficiency_overall.una
# proficiency_oral
qqplot.proficiency_oral.exp <- qplot(sample=pp.explicit$proficiency_oral, stat="qq"); qqplot.proficiency_oral.exp
qqplot.proficiency_oral.inc <- qplot(sample=pp.incidental$proficiency_oral, stat="qq"); qqplot.proficiency_oral.inc
qqplot.proficiency_oral.una <- qplot(sample=pp.unaware$proficiency_oral, stat="qq"); qqplot.proficiency_oral.una
# proficiency_written
qqplot.proficiency_written.exp <- qplot(sample=pp.explicit$proficiency_written, stat="qq"); qqplot.proficiency_written.exp
qqplot.proficiency_written.inc <- qplot(sample=pp.incidental$proficiency_written, stat="qq"); qqplot.proficiency_written.inc
qqplot.proficiency_written.una <- qplot(sample=pp.unaware$proficiency_written, stat="qq"); qqplot.proficiency_written.una
# proficiency_average
qqplot.proficiency_average.exp <- qplot(sample=pp.explicit$proficiency_average, stat="qq"); qqplot.proficiency_average.exp
qqplot.proficiency_average.inc <- qplot(sample=pp.incidental$proficiency_average, stat="qq"); qqplot.proficiency_average.inc
qqplot.proficiency_average.una <- qplot(sample=pp.unaware$proficiency_average, stat="qq"); qqplot.proficiency_average.una
# comprehension_oral
qqplot.comprehension_oral.exp <- qplot(sample=pp.explicit$comprehension_oral, stat="qq"); qqplot.comprehension_oral.exp
qqplot.comprehension_oral.inc <- qplot(sample=pp.incidental$comprehension_oral, stat="qq"); qqplot.comprehension_oral.inc
qqplot.comprehension_oral.una <- qplot(sample=pp.unaware$comprehension_oral, stat="qq"); qqplot.comprehension_oral.una
# comprehension_written
qqplot.comprehension_written.exp <- qplot(sample=pp.explicit$comprehension_written, stat="qq"); qqplot.comprehension_written.exp
qqplot.comprehension_written.inc <- qplot(sample=pp.incidental$comprehension_written, stat="qq"); qqplot.comprehension_written.inc
qqplot.comprehension_written.una <- qplot(sample=pp.unaware$comprehension_written, stat="qq"); qqplot.comprehension_written.una
# PRE_CRIT
qqplot.PRE_CRIT.exp <- qplot(sample=pp.explicit$PRE_CRIT, stat="qq"); qqplot.PRE_CRIT.exp
qqplot.PRE_CRIT.inc <- qplot(sample=pp.incidental$PRE_CRIT, stat="qq"); qqplot.PRE_CRIT.inc
qqplot.PRE_CRIT.una <- qplot(sample=pp.unaware$PRE_CRIT, stat="qq"); qqplot.PRE_CRIT.una
# PRE_CONTR
qqplot.PRE_CONTR.exp <- qplot(sample=pp.explicit$PRE_CONTR, stat="qq"); qqplot.PRE_CONTR.exp
qqplot.PRE_CONTR.inc <- qplot(sample=pp.incidental$PRE_CONTR, stat="qq"); qqplot.PRE_CONTR.inc
qqplot.PRE_CONTR.una <- qplot(sample=pp.unaware$PRE_CONTR, stat="qq"); qqplot.PRE_CONTR.una
# usage
qqplot.usage.exp <- qplot(sample=pp.explicit$usage, stat="qq"); qqplot.usage.exp
qqplot.usage.inc <- qplot(sample=pp.incidental$usage, stat="qq"); qqplot.usage.inc
qqplot.usage.una <- qplot(sample=pp.unaware$usage, stat="qq"); qqplot.usage.una
# age
qqplot.age.exp <- qplot(sample=pp.explicit$age, stat="qq"); qqplot.age.exp
qqplot.age.inc <- qplot(sample=pp.incidental$age, stat="qq"); qqplot.age.inc
qqplot.age.una <- qplot(sample=pp.unaware$age, stat="qq"); qqplot.age.una
# school_evening_years
qqplot.school.exp <- qplot(sample=pp.explicit$school_evening_years, stat="qq"); qqplot.school.exp
qqplot.school.inc <- qplot(sample=pp.incidental$school_evening_years, stat="qq"); qqplot.school.inc
qqplot.school.una <- qplot(sample=pp.unaware$school_evening_years, stat="qq"); qqplot.school.una
# german_at_uni_years
qqplot.uni.exp <- qplot(sample=pp.explicit$german_at_uni_years, stat="qq"); qqplot.uni.exp
qqplot.uni.inc <- qplot(sample=pp.incidental$german_at_uni_years, stat="qq"); qqplot.uni.inc
qqplot.uni.una <- qplot(sample=pp.unaware$german_at_uni_years, stat="qq"); qqplot.uni.una
# amount of L2s (L2s_amount)
qqplot.L2s.exp <- qplot(sample=pp.explicit$L2s_amount, stat="qq"); qqplot.L2s.exp
qqplot.L2s.inc <- qplot(sample=pp.incidental$L2s_amount, stat="qq"); qqplot.L2s.inc
qqplot.L2s.una <- qplot(sample=pp.unaware$L2s_amount, stat="qq"); qqplot.L2s.una

# If you want to save the plots as PNG:
qqplot.lextale.exp
plotwidth = 8
plotheight = 6.5
qq-lextale-exp <-recordPlot() # Save plot information into record
# Create png with width, height and resolution
png("../results/qq-lextale-exp.png", width=plotwidth, height=plotheight,units="in",res=300)
replayPlot(qq-lextale-exp)
dev.off()


# More plots:

library(ggplot2)
# Boxplot
boxplot.lextale <- ggplot(pp.explicit.incidental, aes(learningtype, lextale))
boxplot.lextale + geom_boxplot() + labs(x = "Group", y = "Lextale")

# Density plot
density.lextale.exp <- ggplot(pp.explicit, aes(lextale)) + geom_density() + labs(x= "Lextale", y = "Density"); density.lextale.exp
density.lextale.inc <- ggplot(pp.incidental, aes(lextale)) + geom_density() + labs(x= "Lextale", y = "Density"); density.lextale.inc

# Historgram
histogram.lextale.exp <- ggplot(pp.explicit, aes(lextale)) + geom_histogram(colour="black", fill="#31ADA5", binwidth = 5) + labs(x="Lextale", y = "Density") + theme(legend.position = "none"); histogram.lextale.exp
histogram.lextale.inc <- ggplot(pp.incidental, aes(lextale)) + geom_histogram(colour="black", fill="#31ADA5", binwidth = 5) + labs(x="Lextale", y = "Density") + theme(legend.position = "none"); histogram.lextale.inc

# Histogram with normality curve (from https://www.statmethods.net/graphs/density.html)
hist(pp.explicit$lextale, breaks=12, col="red")
x <- pp.explicit$lextale
h <- hist(x, breaks=10, col="red", xlab="Lextale", main="Histogram with Normal Curve")
xfit <- seq(min(x), max(x), length=40)
yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

# Scatterplot
scatter.lextale <- ggplot(pp.explicit.incidental, aes(lextale, learningtype)) + geom_point() + geom_smooth(method = "lm", colour = "red") + labs(x="Lextale", y = "Group"); scatter.lextale

# Kernel Density plot
d <- density(pp.explicit$lextale)
plot(d)

# Compare groups via Kernal Density
library(sm)
# short version: without legend
sm.density.compare(participantsdata$lextale, participantsdata$learningtype, xlab="Lextale score")
title(main="Group differences for Lextale")
legend(levels=participantsdata$lextale)

# longer version: with legend
attach(participantsdata)
# create value labels
learningtype.f <- factor(learningtype, levels=c("explicit", "incidental", "unaware"),
                         labels=c("explicit", "incidental", "unaware"))
# plot densities
sm.density.compare(lextale, learningtype, xlab="Lextale scores")
title(main="Group differences for Lextale")
# add legend
par(cex = 0.7) # reduce letter size
colfill <- c(2:(2+length(levels(learningtype.f))))
legend("topright", levels(learningtype.f), fill=colfill)


# longer version: with legend
# create value labels
learningtype.f2 <- factor(learningtype, levels=c("explicit", "incidental", "unaware"),
                         labels=c("explicit", "incidental", "unaware"))
# plot densities
sm.density.compare(age, learningtype, xlab="Age")
title(main="Group differences for Age")
# add legend
par(cex = 0.7) # reduce letter size
colfill <- c(2:(2+length(levels(learningtype.f))))
legend("topright", levels(learningtype.f), fill=colfill)




# 3) CHECK HOMOGENEITY OF VARIANCES
###################################

# Homogeneity of variances (between the explicit and incidental groups):
# a) Visual inspection (boxplots)
# b) Levene's test
# c) Fligner-Killeen test
# d) F-test (Stephan Gries 2013, p. 218) [only for normally distributed variables!]


# a) Visual inspection: boxplots

plot(lextale~learningtype, data=pp.explicit.incidental)
plot(age~learningtype, data=pp.explicit.incidental)
plot(L2s_amount~learningtype, data=pp.explicit.incidental)
plot(PRE_CRIT~learningtype, data=pp.explicit.incidental)
plot(PRE_CONTR~learningtype, data=pp.explicit.incidental)
plot(proficiency_overall~learningtype, data=pp.explicit.incidental)
plot(proficiency_oral~learningtype, data=pp.explicit.incidental)
plot(proficiency_written~learningtype, data=pp.explicit.incidental)
plot(proficiency_average~learningtype, data=pp.explicit.incidental)
plot(comprehension_oral~learningtype, data=pp.explicit.incidental)
plot(comprehension_written~learningtype, data=pp.explicit.incidental)
plot(usage~learningtype, data=pp.explicit.incidental)
plot(school_evening_years~learningtype, data=pp.explicit.incidental)
plot(german_at_uni_years~learningtype, data=pp.explicit.incidental)


# b) Levene's test

library(car)
# lextale
levene.lextale <- leveneTest(lextale~learningtype, data=pp.explicit.incidental); levene.lextale
# proficiency_overall
levene.proficiency_overall <- leveneTest(proficiency_overall~learningtype, data=pp.explicit.incidental); levene.proficiency_overall
# proficiency_oral
levene.proficiency_oral <- leveneTest(proficiency_oral~learningtype, data=pp.explicit.incidental); levene.proficiency_oral
# proficiency_written
levene.proficiency_written <- leveneTest(proficiency_written~learningtype, data=pp.explicit.incidental); levene.proficiency_written
# proficiency_average
levene.proficiency_average <- leveneTest(proficiency_average~learningtype, data=pp.explicit.incidental); levene.proficiency_average
# comprehension_oral
levene.comprehension_oral <- leveneTest(comprehension_oral~learningtype, data=pp.explicit.incidental); levene.comprehension_oral
# comprehension_written
levene.comprehension_written <- leveneTest(comprehension_written~learningtype, data=pp.explicit.incidental); levene.comprehension_written
# PRE_CRIT
levene.PRE_CRIT <- leveneTest(PRE_CRIT~learningtype, data=pp.explicit.incidental); levene.PRE_CRIT 
# PRE_CONTR
levene.PRE_CONTR <- leveneTest(PRE_CONTR~learningtype, data=pp.explicit.incidental); levene.PRE_CONTR 
# usage
levene.usage <- leveneTest(usage~learningtype, data=pp.explicit.incidental); levene.usage
# age
levene.age <- leveneTest(age~learningtype, data=pp.explicit.incidental); levene.age
# L2s_amount
levene.L2s <- leveneTest(L2s_amount~learningtype, data=pp.explicit.incidental); levene.L2s
# school_evening_years
levene.school <- leveneTest(school_evening_years~learningtype, data=pp.explicit.incidental); levene.school
# usage
levene.uni <- leveneTest(german_at_uni_years~learningtype, data=pp.explicit.incidental); levene.uni

# Bind the results together:
levene.results <- cbind(levene.lextale,levene.proficiency_overall,levene.proficiency_oral,
                        levene.proficiency_written,levene.proficiency_average,levene.comprehension_oral,
                        levene.comprehension_written,levene.PRE_CONTR,levene.PRE_CRIT, levene.usage,
                        levene.age, levene.L2s, levene.school, levene.uni)

write.csv(levene.results, file="Levene.csv") # write csv output
# Attention: not nice output; doesn't show variable names.


# c) Fligner-Killeen test (non-parametric)

# lextale
fligner.lextale <- fligner.test(lextale~learningtype, data=pp.explicit.incidental);fligner.lextale
# proficiency_overall
fligner.proficiency_overall <- fligner.test(proficiency_overall~learningtype, data=pp.explicit.incidental);fligner.proficiency_overall
# proficiency_oral
fligner.proficiency_oral <- fligner.test(proficiency_oral~learningtype, data=pp.explicit.incidental);fligner.proficiency_oral
# proficiency_written
fligner.proficiency_written <- fligner.test(proficiency_written~learningtype, data=pp.explicit.incidental);fligner.proficiency_written
# proficiency_average
fligner.proficiency_average <- fligner.test(proficiency_average~learningtype, data=pp.explicit.incidental);fligner.proficiency_average
# comprehension_oral
fligner.comprehension_oral <- fligner.test(comprehension_oral~learningtype, data=pp.explicit.incidental);fligner.comprehension_oral
# comprehension_written
fligner.comprehension_written <- fligner.test(comprehension_written~learningtype, data=pp.explicit.incidental);fligner.comprehension_written
# PRE_CRIT
fligner.PRE_CRIT <- fligner.test(PRE_CRIT~learningtype, data=pp.explicit.incidental);fligner.PRE_CRIT
# PRE_CONTR
fligner.PRE_CONTR <- fligner.test(PRE_CONTR~learningtype, data=pp.explicit.incidental);fligner.PRE_CONTR
# usage
fligner.usage <- fligner.test(usage~learningtype, data=pp.explicit.incidental);fligner.usage
# age
fligner.age <- fligner.test(age~learningtype, data=pp.explicit.incidental);fligner.age
# L2 amount
fligner.L2s <- fligner.test(L2s_amount~learningtype, data=pp.explicit.incidental);fligner.L2s
# school
fligner.school <- fligner.test(school_evening_years~learningtype, data=pp.explicit.incidental);fligner.school
# uni
fligner.uni <- fligner.test(german_at_uni_years~learningtype, data=pp.explicit.incidental);fligner.uni

# Bind the results together:
fligner.results <- cbind(fligner.lextale,fligner.proficiency_overall,fligner.proficiency_oral,
                         fligner.proficiency_written,fligner.proficiency_average,fligner.comprehension_oral,
                         fligner.comprehension_written,fligner.PRE_CONTR,fligner.PRE_CRIT, fligner.usage,
                         fligner.age, fligner.L2s, fligner.school, fligner.uni)

write.csv(fligner.results, file="Fligner.csv") # write csv output


# d) F-test (Stephan Gries 2013, p. 218) [only for normally distributed variables!]

# use var.test()
# Output interpretation: 1) Is p > .05?; 2) Does the CI include 1?

F.lextale <- var.test(pp.explicit.incidental$lextale~pp.explicit.incidental$learningtype); F.lextale


# 4) TESTS FOR GROUP DIFFERENCES
################################

# Different possibilities:

# For numeric (ratio-scaled) outcome variables:
# a) Independent samples T-test after Welch
# b) Robust Wilcox Method
# c) Wilcoxon rank-sum test (= Mann-Whitney U test) [Non-parametric alternative if normality is violated]

# For ordinal outcome variables (self-rated measures):
# c) Mann-Whitney U-test

# For categorical outcome variables:
# d) Pearson's Chi Squared test with Yate's continuity correction 

# e) Compare overall distributions: Kolmogorov-Smirnov



# a) Independent samples T-test

# Info: Source: Larson-Hall 2016, p.294
# Only use this test for ratio-scaled data and if the assumptions of normality is met. Homogeneity of variances is not an issue, since this is the t-test after Welch.

# lextale
t.test.lextale <- t.test(lextale~learningtype, paired=FALSE, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental); t.test.lextale
# PRE_CRIT
t.test.PRE_CRIT <- t.test(PRE_CRIT~learningtype, paired=FALSE, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental); t.test.PRE_CRIT
# PRE_CONTR
t.test.PRE_CONTR <- t.test(PRE_CONTR~learningtype, paired=FALSE, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental); t.test.PRE_CONTR 
# age
t.test.age <- t.test(age~learningtype, paired=FALSE, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental); t.test.age
# L2s_amount
t.test.L2s_amount <- t.test(L2s_amount~learningtype, paired=FALSE, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental); t.test.L2s_amount
# school_evening_years
t.test.school <- t.test(school_evening_years~learningtype, paired=FALSE, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental); t.test.school_evening_years
# german_at_uni_years
t.test.uni <- t.test(german_at_uni_years~learningtype, paired=FALSE, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental); t.test.uni
# proficiency_average
t.test.proficiency_average <- t.test(proficiency_average~learningtype, paired=FALSE, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental); t.test.proficiency_average


# bind results together
t.tests<-cbind(t.test.lextale, t.test.PRE_CRIT, t.test.PRE_CONTR, t.test.age, t.test.L2s_amount, t.test.school, t.test.uni, t.test.proficiency_average)

write.csv(t.tests, file="T-tests.csv") # write csv output


# b) Robust Wilcox Method

# Info: recommended and described by Field, Miles & Fiels (2012) and Larson-Hall (2016, p.296)
# However, I also heard opinions that you should not start transforming your data when sample sizes are small.
# If you apply mean trimming + bootstrapping, you should also report the trimmed means.

library(WRS2) #WRS2 v0.9-2, by Patrick Mair 
citation(package="WRS2")

# Different options:
yuen(lextale~learningtype, data=pp.explicit.incidental, tr = 0.2) # basic function with 20% trimmed means (default)
yuenbt(lextale~learningtype, data=pp.explicit.incidental, tr = 0.2, nboot=10000) # trimmed means + bootstrapping
robust.t.test.lextale <- yuenbt(lextale~learningtype, data=pp.explicit.incidental, tr = 0.2, nboot=10000); robust.t.test.lextale # example with Lextale

# Robust effect sizes (akp):
# (Extracted from WRS2 document: https://cran.r-project.org/web/packages/WRS2/vignettes/WRS2.pdf)
  # "In terms of effect size, Algina, Keselman, and Penfield (2005) presented a robust version of
  # Cohens d (Cohen 1988) based on 20% trimmed means and Winsorized variances."
  # "The same rules of thumb as for Cohens d can be used; that is, d = 0.2, 0.5, and 0.8
  # correspond to small, medium, and large effects."
lextale.d <- akp.effect(lextale~learningtype, data=pp.explicit.incidental)

# Robust effect sizes (yuen):
  # It can happen that the two effect sizes do not lead to the same conclusions about the strength
  # of the effect ... Wilcox and Tian (2011) proposed an explanatory measure of effect size which
  # does not suffer from this shortcoming and is generalizable to multiple groups.
lextale.d2 <- yuen.effect.ci(lextale~learningtype, data=pp.explicit.incidental)

# Get the trimmed means:
mean.lextale.exp <- mean(pp.explicit$lextale, tr=.2); mean.lextale.exp
mean.lextale.inc <- mean(pp.incidental$lextale, tr=.2); mean.lextale.inc


# c) Wilcoxon rank-sum test (= Mann-Whitney U test) [Non-parametric alternative if normality is violated, or if you have ordinal data]

# Continuous variables: ratio-scaled

# lextale
wilcox.lextale <- wilcox.test(lextale~learningtype, data=pp.explicit.incidental, paired=FALSE); wilcox.lextale
# PRE_CRIT
wilcox.PRE_CRIT <- wilcox.test(PRE_CRIT~learningtype, data=pp.explicit.incidental, paired=FALSE); wilcox.PRE_CRIT
# PRE_CONTR
wilcox.PRE_CONTR <- wilcox.test(PRE_CONTR~learningtype, data=pp.explicit.incidental, paired=FALSE); wilcox.PRE_CONTR
# age
wilcox.age <- wilcox.test(age~learningtype, data=pp.explicit.incidental, paired=FALSE); wilcox.age
# L2s_amount
wilcox.L2s <- wilcox.test(L2s_amount~learningtype, data=pp.explicit.incidental, paired=FALSE); wilcox.L2s
# proficiency_average
wilcox.proficiency_average <- wilcox.test(proficiency_average~learningtype, data=pp.explicit.incidental, paired=FALSE); wilcox.proficiency_average
# school_evening_years
wilcox.school <- wilcox.test(school_evening_years~learningtype, data=pp.explicit.incidental, paired=FALSE); wilcox.school
# german_at_uni_years
wilcox.uni <- wilcox.test(german_at_uni_years~learningtype, data=pp.explicit.incidental, paired=FALSE); wilcox.uni

# Self-rated variables: ordinal

# proficiency_overall
wilcox.proficiency_overall <- wilcox.test(proficiency_overall~learningtype, data=pp.explicit.incidental, paired=FALSE); wilcox.proficiency_overall
# proficiency_oral
wilcox.proficiency_oral <- wilcox.test(proficiency_oral~learningtype, data=pp.explicit.incidental, paired=FALSE); wilcox.proficiency_oral
# proficiency_written
wilcox.proficiency_written <- wilcox.test(proficiency_written~learningtype, data=pp.explicit.incidental, paired=FALSE); wilcox.proficiency_written
# comprehension_oral
wilcox.comprehension_oral <- wilcox.test(comprehension_oral~learningtype, data=pp.explicit.incidental, paired=FALSE); wilcox.comprehension_oral
# comprehension_written
wilcox.comprehension_written <- wilcox.test(comprehension_written~learningtype, data=pp.explicit.incidental, paired=FALSE); wilcox.comprehension_written
# usage
wilcox.usage <- wilcox.test(usage~learningtype, data=pp.explicit.incidental, paired=FALSE); wilcox.usage

wilcox.results<-cbind(wilcox.lextale,wilcox.PRE_CRIT,wilcox.PRE_CONTR,wilcox.usage,wilcox.age,wilcox.L2s,wilcox.school,wilcox.uni,
                      wilcox.proficiency_overall,wilcox.proficiency_oral,wilcox.proficiency_written,
                      wilcox.proficiency_average,wilcox.comprehension_oral,wilcox.comprehension_written)

write.csv(wilcox.results, file="Wilcox_tests.csv")


# Effect size (approximate) for Wilcoxon rank-sum test

# Write the function (from Field, Miles & Fiels 2012, p.665)
rWilcox <- function(wilcoxModel, N){
  z <- qnorm(wilcoxModel$p.value/2)
  r <- z/sqrt(N)
  cat(wilcoxModel$data.name, "Effect size, r = ", r)
}

# Formula: rWilcox(modelx, total sample size)
rWilcox(wilcox.lextale, 42)
rWilcox(wilcox.age, 42)
rWilcox(wilcox.L2s, 42)
rWilcox(wilcox.PRE_CRIT, 42)
rWilcox(wilcox.PRE_CONTR, 42)
rWilcox(wilcox.proficiency_average, 42)
rWilcox(wilcox.school, 42)
rWilcox(wilcox.uni, 42)

rWilcox(wilcox.proficiency_overall, 42)
rWilcox(wilcox.proficiency_oral, 42)
rWilcox(wilcox.proficiency_written, 42)
rWilcox(wilcox.comprehension_oral, 42)
rWilcox(wilcox.comprehension_written, 42)
rWilcox(wilcox.usage, 42)


# Alternative effect size for Wilcox:
# The estimator that corresponds to the Wilcoxon test is the Hodges-Lehmann estimator;
# it's returned by wilcox.test using the conf.int=TRUE option, under "difference in location".


# d) Pearson's Chi Squared test with Yate's continuity correction

library(gmodels)
# CrossTable output: look at "Pearson's Chi-squared test with Yates' continuity correction" (because we have a 2x2 contingency table)
# Assumption: expected frequencies should be greater than 5 (Field, p. 801).

# bilingualism
CrossTable(pp.explicit.incidental$learningtype, pp.explicit.incidental$bilingualism, fisher=TRUE, expected=TRUE, sresid=TRUE, format = "SPSS") 

# Effect size: Odds ratio

# Calculation by hand:
# odds (for explicit learners being bilingual) = number of bilinguals (8)/ number of monolinguals (13)
# odds (for incidental learners being bilingual) = number of bilinguals (3) / number of monolinguals (18)
# odds ratio = odds(for explicit learners being bilingual) / odds(for incidental learners being bilingual)
(8/13)/(3/18) # = 3.692308
# 3.69 = Meaning that the explicit learners are 3.69 times more likely to be bilingual than the incidental learners.
# The odds ratio is also returned by CrossTable if we set fisher = TRUE: giver odds ratio and its CI.
# Interpretation:
# A value of 1 means that the odds for explicit and incidental learners to be bilingual are equal.
# A value < 1 means that the odds for explicit learners to be bilingual are smaller than for incidental learners to be bilingual.
# A value > 1 means that the odds for explicit learners to be bilingual are greater than for incidental learners to be bilingual.
# The CI should not cross 1.

# L11 (Flemish vs. Dutch from the Netherlands)
CrossTable(pp.explicit.incidental$learningtype, pp.explicit.incidental$L11, fisher=TRUE, expected=TRUE, sresid=TRUE, format = "SPSS") 


# e) Compare overall distributions: two-sample Kolmogorov-Smirnov

# Recommended and described by Gries 2013, p.172
# We look at overall distributions, not just means or just variances. It is possible that the means are very similar while the variances are not
# and a test for different means might not uncover the overall distributional difference.

# DV: ordinal/interval/ratio-scaled; IV: nominal; independent samples

# Test: two-sample Kolmogorov-Smirnov
attach(pp.explicit.incidental)
ks.test(lextale[learningtype=="explicit"], lextale[learningtype=="incidental"])
ks.test(age[learningtype=="explicit"], age[learningtype=="incidental"])
ks.test(PRE_CRIT[learningtype=="explicit"], PRE_CRIT[learningtype=="incidental"])
ks.test(PRE_CONTR[learningtype=="explicit"], PRE_CONTR[learningtype=="incidental"])
ks.test(L2s_amount[learningtype=="explicit"], L2s_amount[learningtype=="incidental"])
ks.test(school_evening_years[learningtype=="explicit"], school_evening_years[learningtype=="incidental"])
ks.test(german_at_uni_years[learningtype=="explicit"], german_at_uni_years[learningtype=="incidental"])

# Self-rated variables: Only works if you use them as numerical variables.
ks.test(proficiency_average[learningtype=="explicit"], proficiency_average[learningtype=="incidental"])
ks.test(proficiency_overall[learningtype=="explicit"], proficiency_overall[learningtype=="incidental"])
ks.test(proficiency_oral[learningtype=="explicit"], proficiency_oral[learningtype=="incidental"])
ks.test(proficiency_written[learningtype=="explicit"], proficiency_written[learningtype=="incidental"])
ks.test(comprehension_oral[learningtype=="explicit"], comprehension_oral[learningtype=="incidental"])
ks.test(usage[learningtype=="explicit"], usage[learningtype=="incidental"])
detach(pp.explicit.incidental)




# # # # # # # # #
# end of script #
# # # # # # # # #

