########################################
# RULE OUT A PRIORI GROUP DIFFERENCES:
########################################
# Script written in May 2018 to analyse the Study 1 data. More specifically to check if there
# were any ?? priori group differences between the explicit, incidental and unaware groups.


rm(list=ls()) # clear workspace
options(contrasts=c("contr.sum","contr.poly")) # Set contrasts to sum-to-zero
# set working directory to source file location

# PREPARE DATASETS:
participantsdata <- read.csv("../data/180529_Data_participants_IV.csv", header = TRUE) # load data
colnames(participantsdata)[1] <- "participant" #rename participant variable
str(participantsdata)

# turn self-rated variables into ordered factor:
#participantsdata$proficiency_overall <- as.factor(participantsdata$proficiency_overall)
proficiency_overall <- factor(participantsdata$proficiency_overall, ordered=TRUE)
proficiency_written <- factor(participantsdata$proficiency_written, ordered=TRUE)
proficiency_average <- factor(participantsdata$proficiency_average, ordered=TRUE)
proficiency_oral <- factor(participantsdata$proficiency_oral, ordered=TRUE)
comprehension_oral <- factor(participantsdata$comprehension_oral, ordered=TRUE)
comprehension_written <- factor(participantsdata$comprehension_written, ordered=TRUE)
usage <- factor(participantsdata$usage, ordered=TRUE)

# subsets:
pp.explicit <- subset(participantsdata, subset=learningtype=="explicit")
pp.incidental <- subset(participantsdata, subset=learningtype=="incidental")
pp.unaware <- subset(participantsdata, subset=learningtype=="unaware")

# Separately load data about instruction time
instructiontime <- read.csv("../data/180522_German_instruction.csv", header = TRUE)
colnames(instructiontime)[1] <- "participant" #rename participant variable
instruction.explicit <- subset(instructiontime, subset=Learningtype=="explicit")
instruction.incidental <- subset(instructiontime, subset=Learningtype=="incidental")
instruction.unaware <- subset(instructiontime, subset=Learningtype=="unaware")

# Trying to drop unused levels, but doesn't currently work
instruction.exp.inc <- subset(instructiontime, Learningtype !="unaware")
instruction.exp.inc <- droplevels(instruction.exp.inc, instruction.exp.inc$Learningtype=="unaware")

# Variables to check:
colnames(participantsdata)
# - grouping variable: learningtype
# - lextale
# - proficiency_overall
# - proficiency_oral
# - proficiency_written
# - proficiency_average
# - comprehension_oral
# - comprehension_written
# - PRE_CRIT
# - PRE_CONTR
# - usage

# and more!



### FIRST: CHECK NORMALITY, PER VARIABLE AND PER GROUP! ###
###########################################################

# NORMALITY:
# Shapiro-Wilk test
# Skew and kurtosis values
# Visual data inspection
# - QQ-Plot
# - Boxplots
# - Histogram with normality line



# SHAPIRO-WILK (should not be significant)
# lextale
shapiro.lextale.exp <- shapiro.test(pp.explicit$lextale)
shapiro.lextale.inc <- shapiro.test(pp.incidental$lextale)
shapiro.lextale.una <- shapiro.test(pp.unaware$lextale)
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
shapiro.proficiency_average.exp <- shapiro.test(pp.explicit$proficiency_average)
shapiro.proficiency_average.inc <- shapiro.test(pp.incidental$proficiency_average)
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
shapiro.PRE_CRIT.exp <- shapiro.test(pp.explicit$PRE_CRIT)
shapiro.PRE_CRIT.inc <- shapiro.test(pp.incidental$PRE_CRIT)
shapiro.PRE_CRIT.una <- shapiro.test(pp.unaware$PRE_CRIT) # error (I'll exclude it from the list)
# PRE_CONTR
shapiro.PRE_CONTR.exp <- shapiro.test(pp.explicit$PRE_CONTR)
shapiro.PRE_CONTR.inc <- shapiro.test(pp.incidental$PRE_CONTR)
shapiro.PRE_CONTR.una <- shapiro.test(pp.unaware$PRE_CONTR)
# usage
shapiro.usage.exp <- shapiro.test(pp.explicit$usage)
shapiro.usage.inc <- shapiro.test(pp.incidental$usage)
shapiro.usage.una <- shapiro.test(pp.unaware$usage)


# bind results together
shapiro.results <- cbind(shapiro.lextale.exp, shapiro.lextale.inc, shapiro.lextale.una, shapiro.proficiency_overall.exp, 
                         shapiro.proficiency_overall.inc, shapiro.proficiency_overall.una, shapiro.proficiency_oral.exp,
                         shapiro.proficiency_oral.inc, shapiro.proficiency_oral.una, shapiro.proficiency_written.exp,
                         shapiro.proficiency_written.inc, shapiro.proficiency_written.una, shapiro.proficiency_average.exp, 
                         shapiro.proficiency_average.inc, shapiro.proficiency_average.una, shapiro.comprehension_oral.exp, 
                         shapiro.comprehension_oral.inc, shapiro.comprehension_oral.una, shapiro.comprehension_written.exp,
                         shapiro.comprehension_written.inc, shapiro.comprehension_written.una, shapiro.PRE_CRIT.exp, shapiro.PRE_CRIT.inc,
                         shapiro.PRE_CONTR.exp, shapiro.PRE_CONTR.inc, shapiro.PRE_CONTR.una,
                         shapiro.usage.exp, shapiro.usage.inc, shapiro.usage.una)

write.csv(shapiro.results, file="../results/Shapiro.csv") # write csv output


# CHECK SKEW AND KURTOSIS (skew.2SE should be smaller than 1)
library(pastecs)
# lextale
desc.lextale.exp <- stat.desc(pp.explicit$lextale, basic=TRUE, norm=TRUE)
desc.lextale.inc <- stat.desc(pp.incidental$lextale, basic=TRUE, norm=TRUE)
desc.lextale.una <- stat.desc(pp.unaware$lextale, basic=TRUE, norm=TRUE)
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
desc.proficiency_average.exp <- stat.desc(pp.explicit$proficiency_average, basic=TRUE, norm=TRUE)
desc.proficiency_average.inc <- stat.desc(pp.incidental$proficiency_average, basic=TRUE, norm=TRUE)
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
desc.PRE_CRIT.exp <- stat.desc(pp.explicit$PRE_CRIT, basic=TRUE, norm=TRUE)
desc.PRE_CRIT.inc <- stat.desc(pp.incidental$PRE_CRIT, basic=TRUE, norm=TRUE)
desc.PRE_CRIT.una <- stat.desc(pp.unaware$PRE_CRIT, basic=TRUE, norm=TRUE) # error (I'll exclude it from the list)
# PRE_CONTR
desc.PRE_CONTR.exp <- stat.desc(pp.explicit$PRE_CONTR, basic=TRUE, norm=TRUE)
desc.PRE_CONTR.inc <- stat.desc(pp.incidental$PRE_CONTR, basic=TRUE, norm=TRUE)
desc.PRE_CONTR.una <- stat.desc(pp.unaware$PRE_CONTR, basic=TRUE, norm=TRUE)
# usage
desc.usage.exp <- stat.desc(pp.explicit$usage, basic=TRUE, norm=TRUE)
desc.usage.inc <- stat.desc(pp.incidental$usage, basic=TRUE, norm=TRUE)
desc.usage.una <- stat.desc(pp.unaware$usage, basic=TRUE, norm=TRUE)
desc.usage.exp
desc.usage.inc
desc.usage.una 

# bind results together
desc.results <- cbind(desc.lextale.exp,desc.lextale.inc,desc.lextale.una,
                      desc.proficiency_overall.exp,desc.proficiency_overall.inc,desc.proficiency_overall.una,
                      desc.proficiency_oral.exp,desc.proficiency_oral.inc,desc.proficiency_oral.una,
                      desc.proficiency_written.exp,desc.proficiency_written.inc,desc.proficiency_written.una,
                      desc.proficiency_average.exp,desc.proficiency_average.inc,desc.proficiency_average.una,
                      desc.comprehension_oral.exp,desc.comprehension_oral.inc,desc.comprehension_oral.una,
                      desc.comprehension_written.exp,desc.comprehension_written.inc,desc.comprehension_written.una,
                      desc.PRE_CRIT.exp,desc.PRE_CRIT.inc, desc.PRE_CONTR.exp,desc.PRE_CONTR.inc,desc.PRE_CONTR.una,
                      desc.usage.exp, desc.usage.inc, desc.usage.una)

write.csv(desc.results, file="../results/Skew_and_kurtosis.csv") # write csv output


# QQ-PLOTS (dots should be on a straight line)
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

# SAVING PLOT AS PNG:
qqplot.lextale.exp

plotwidth = 8
plotheight = 6.5

qq-lextale-exp <-recordPlot() # Save plot information into record

# Create png with width, height and resolution
png("../results/qq-lextale-exp.png", width=plotwidth, height=plotheight,units="in",res=300)
replayPlot(qq-lextale-exp)
dev.off()



# RESTRUCTURE DATA FOR NEXT STEPS
# some tests only work if the grouping factor has 2 levels.
pp.explicit.incidental <- subset(participantsdata, learningtype != "unaware") #drop the "unaware" learners from the dataset
levels(pp.explicit.incidental$learningtype) # although the rows have gone, the factor still has the three levels in it
pp.explicit.incidental <- droplevels(pp.explicit.incidental, pp.explicit.incidental$learningtype=="unaware") # drop the unused factor level
levels(pp.explicit.incidental$learningtype) # OK!



### SECOND: CHECK HOMOGENEITY OF VARIANCES ###
##############################################

# visual inspection: boxplots
plot(lextale~learningtype, data=pp.explicit.incidental)

# LEVENE'S TEST
library(car)
# lextale
levene.lextale <- leveneTest(lextale~learningtype, data=pp.explicit.incidental)
levene.lextale
# proficiency_overall
levene.proficiency_overall <- leveneTest(proficiency_overall~learningtype, data=pp.explicit.incidental)
# proficiency_oral
levene.proficiency_oral <- leveneTest(proficiency_oral~learningtype, data=pp.explicit.incidental)
# proficiency_written
levene.proficiency_written <- leveneTest(proficiency_written~learningtype, data=pp.explicit.incidental)
# proficiency_average
levene.proficiency_average <- leveneTest(proficiency_average~learningtype, data=pp.explicit.incidental)
# comprehension_oral
levene.comprehension_oral <- leveneTest(comprehension_oral~learningtype, data=pp.explicit.incidental)
# comprehension_written
levene.comprehension_written <- leveneTest(comprehension_written~learningtype, data=pp.explicit.incidental)
# PRE_CRIT
levene.PRE_CRIT <- leveneTest(PRE_CRIT~learningtype, data=pp.explicit.incidental)
# PRE_CONTR
levene.PRE_CONTR <- leveneTest(PRE_CONTR~learningtype, data=pp.explicit.incidental)
# usage
levene.usage <- leveneTest(usage~learningtype, data=pp.explicit.incidental)

levene.results <- cbind(levene.lextale,levene.proficiency_overall,levene.proficiency_oral,
                        levene.proficiency_written,levene.proficiency_average,levene.comprehension_oral,
                        levene.comprehension_written,levene.PRE_CONTR,levene.PRE_CRIT, levene.usage)

write.csv(levene.results, file="Levene.csv") # write csv output
# Attention: not nice output; doesn't show variable names



# FLIGNER-KILLEEN TEST (non-parametric)
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
# L2 amount
fligner.L2s <- fligner.test(L2s_amount~learningtype, data=pp.explicit.incidental);fligner.L2s
# school
fligner.school <- fligner.test(School_Evening_years~Learningtype, data=instruction.exp.inc);fligner.school
# uni
fligner.uni <- fligner.test(German_at_uni_years~Learningtype, data=instruction.exp.inc);fligner.uni



fligner.results <- cbind(fligner.lextale,fligner.proficiency_overall,fligner.proficiency_oral,
                         fligner.proficiency_written,fligner.proficiency_average,fligner.comprehension_oral,
                         fligner.comprehension_written,fligner.PRE_CONTR,fligner.PRE_CRIT, fligner.usage)

write.csv(fligner.results, file="Fligner.csv") # write csv output





### COMPARE THE EXPLICIT AND INCIDENTAL GROUPS ###
##################################################



## T-TEST (independent samples; LH p.294) ##
############################################

# lextale
t.test.lextale <- t.test(lextale~learningtype, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental)
t.test.lextale
# proficiency_overall
t.test.proficiency_overall <- t.test(proficiency_overall~learningtype, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental)
# proficiency_oral
t.test.proficiency_oral <- t.test(proficiency_oral~learningtype, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental)
# proficiency_written
t.test.proficiency_written <- t.test(proficiency_written~learningtype, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental)
# proficiency_average
t.test.proficiency_average <- t.test(proficiency_average~learningtype, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental)
# comprehension_oral
t.test.comprehension_oral <- t.test(comprehension_oral~learningtype, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental)
# comprehension_written
t.test.comprehension_written <- t.test(comprehension_written~learningtype, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental)
# PRE_CRIT
t.test.PRE_CRIT <- t.test(PRE_CRIT~learningtype, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental)
# PRE_CONTR
t.test.PRE_CONTR <- t.test(PRE_CONTR~learningtype, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental)
# usage
t.test.usage <- t.test(usage~learningtype, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental)

# bind results together
t.tests<-cbind(t.test.lextale,t.test.proficiency_overall,t.test.proficiency_oral,t.test.proficiency_written,
               t.test.proficiency_average,t.test.comprehension_oral,t.test.comprehension_written,t.test.PRE_CRIT,
               t.test.PRE_CONTR, t.test.usage)

write.csv(t.tests, file="T-tests.csv") # write csv output



## ROBUST WILCOX METHOD (in Field; LH p.296) ##
###############################################
library(WRS2) #WRS2 v0.9-2, by Patrick Mair 

citation(package="WRS2")

# different options:
yuen(lextale~learningtype, data=pp.explicit.incidental, tr = 0.2) # basic function with 20% trimmed means (default)
yuenbt(lextale~learningtype, data=pp.explicit.incidental, tr = 0.2, nboot=10000) # trimmed means + bootstrapping
robust.t.test.lextale <- yuenbt(lextale~learningtype, data=pp.explicit.incidental, tr = 0.2, nboot=10000)
robust.t.test.lextale

# for now, I decide to get 20% trimmed means + bootstrapping:

# age
robust.ttest.age <- yuenbt(age~learningtype, data=pp.explicit.incidental, tr = 0.2, nboot=10000); robust.ttest.age
# lextale
robust.ttest.lextale <- yuenbt(lextale~learningtype, data=pp.explicit.incidental, tr = 0.2, nboot=10000)
# proficiency_overall
robust.ttest.proficiency_overall <- yuenbt(proficiency_overall~learningtype, data=pp.explicit.incidental, tr = 0.2, nboot=10000)
# proficiency_oral
robust.ttest.proficiency_oral <- yuenbt(proficiency_oral~learningtype, data=pp.explicit.incidental, tr = 0.2, nboot=10000)
# proficiency_written
robust.ttest.proficiency_written <- yuenbt(proficiency_written~learningtype, data=pp.explicit.incidental, tr = 0.2, nboot=10000)
# proficiency_average
robust.ttest.proficiency_average <- yuenbt(proficiency_average~learningtype, data=pp.explicit.incidental, tr = 0.2, nboot=10000)
# comprehension_oral
robust.ttest.comprehension_oral <- yuenbt(comprehension_oral~learningtype, data=pp.explicit.incidental, tr = 0.2, nboot=10000)
# comprehension_written
robust.ttest.comprehension_written <- yuenbt(comprehension_written~learningtype, data=pp.explicit.incidental, tr = 0.2, nboot=10000)
# PRE_CRIT
robust.ttest.PRE_CRIT <- yuenbt(PRE_CRIT~learningtype, data=pp.explicit.incidental, tr = 0.2, nboot=10000)
# PRE_CONTR
robust.ttest.PRE_CONTR <- yuenbt(PRE_CONTR~learningtype, data=pp.explicit.incidental, tr = 0.2, nboot=10000)
# usage
robust.ttest.usage <- yuenbt(usage~learningtype, data=pp.explicit.incidental, tr = 0.2, nboot=10000)
# amount of L2s
robust.ttest.L2s_amount <- yuenbt(L2s_amount~learningtype, data=pp.explicit.incidental, tr = 0.2, nboot=10000); robust.ttest.L2s_amount
# years of instruction at school & evening class
robust.ttest.school <- yuenbt(School_Evening_years~Learningtype, data=instruction.exp.inc, tr =0.2, nboot=10000); robust.ttest.school
# years of German at uni
robust.ttest.uni <- yuenbt(German_at_uni_years~Learningtype, data=instruction.exp.inc, tr =0.2, nboot=10000); robust.ttest.uni


robust.ttest.results <- cbind(robust.ttest.lextale,robust.ttest.proficiency_overall,robust.ttest.proficiency_oral,
                              robust.ttest.proficiency_written,robust.ttest.proficiency_average,robust.ttest.comprehension_oral,
                              robust.ttest.comprehension_written,robust.ttest.PRE_CRIT,robust.ttest.PRE_CONTR,
                              robust.ttest.usage)

write.csv(robust.ttest.results, file="Robust_t-tests.csv")


# Robust effect size: From WRS2 document #
##########################################

# In terms of effect size, Algina, Keselman, and Penfield (2005) presented a robust version of
# Cohen???s d (Cohen 1988) based on 20% trimmed means and Winsorized variances.

# The same rules of thumb as for Cohen???s d can be used; that is, |d| = 0.2, 0.5, and 0.8
# correspond to small, medium, and large effects. 

age.d <- akp.effect(age~learningtype, data=pp.explicit.incidental)
lextale.d <- akp.effect(lextale~learningtype, data=pp.explicit.incidental)
proficiency_overall.d <- akp.effect(proficiency_overall~learningtype, data=pp.explicit.incidental)
proficiency_oral.d <- akp.effect(proficiency_oral~learningtype, data=pp.explicit.incidental)
proficiency_written.d <- akp.effect(proficiency_written~learningtype, data=pp.explicit.incidental)
proficiency_average.d <- akp.effect(proficiency_average~learningtype, data=pp.explicit.incidental)
comprehension_oral.d <- akp.effect(comprehension_oral~learningtype, data=pp.explicit.incidental)
comprehension_written.d <- akp.effect(comprehension_written~learningtype, data=pp.explicit.incidental)
PRE_CRIT.d <- akp.effect(PRE_CRIT~learningtype, data=pp.explicit.incidental)
PRE_CONTR.d <- akp.effect(PRE_CONTR~learningtype, data=pp.explicit.incidental)
usage.d <- akp.effect(usage~learningtype, data=pp.explicit.incidental)
L2s_amount.d <- akp.effect(L2s_amount~learningtype, data=pp.explicit.incidental)
school.d <- akp.effect(School_Evening_years~Learningtype, data=instruction.exp.inc)
uni.d <- akp.effect(German_at_uni_years~Learningtype, data=instruction.exp.inc)

robust.d <- c(age.d, lextale.d, proficiency_overall.d, proficiency_oral.d, proficiency_written.d, proficiency_average.d,
              comprehension_oral.d, comprehension_written.d, PRE_CRIT.d, PRE_CONTR.d, usage.d, L2s_amount.d, school.d, uni.d)

write.csv(robust.d, file="robust_effect_sizes.csv")


# It can happen that the two effect sizes do not lead to the same conclusions about the strength
# of the effect (as in our example to a certain extent). Wilcox and Tian (2011) proposed
# an explanatory measure of effect size ?? which does not suffer from this shortcoming and is
# generalizable to multiple groups.
age.d2 <- yuen.effect.ci(age~learningtype, data=pp.explicit.incidental)
lextale.d2 <- yuen.effect.ci(lextale~learningtype, data=pp.explicit.incidental)
proficiency_overall.d2 <- yuen.effect.ci(proficiency_overall~learningtype, data=pp.explicit.incidental)
proficiency_oral.d2 <- yuen.effect.ci(proficiency_oral~learningtype, data=pp.explicit.incidental)
proficiency_written.d2 <- yuen.effect.ci(proficiency_written~learningtype, data=pp.explicit.incidental)
proficiency_average.d2 <- yuen.effect.ci(proficiency_average~learningtype, data=pp.explicit.incidental)
comprehension_oral.d2 <- yuen.effect.ci(comprehension_oral~learningtype, data=pp.explicit.incidental)
comprehension_written.d2 <- yuen.effect.ci(comprehension_written~learningtype, data=pp.explicit.incidental)
PRE_CRIT.d2 <- yuen.effect.ci(PRE_CRIT~learningtype, data=pp.explicit.incidental)
PRE_CONTR.d2 <- yuen.effect.ci(PRE_CONTR~learningtype, data=pp.explicit.incidental)
usage.d2 <- yuen.effect.ci(usage~learningtype, data=pp.explicit.incidental)
L2s_amount.d2 <- yuen.effect.ci(L2s_amount~learningtype, data=pp.explicit.incidental)
school.d2 <- yuen.effect.ci(School_Evening_years~Learningtype, data=instruction.exp.inc)
uni.d2 <- yuen.effect.ci(German_at_uni_years~Learningtype, data=instruction.exp.inc)

robust.d2 <- c(age.d2, lextale.d2, proficiency_overall.d2, proficiency_oral.d2, proficiency_written.d2, proficiency_average.d2,
              comprehension_oral.d2, comprehension_written.d2, PRE_CRIT.d2, PRE_CONTR.d2, usage.d2, L2s_amount.d2, school.d2, uni.d2)

write.csv(robust.d2, file="robust_effect_sizes2.csv")




# Get the trimmed means:
mean.lextale.exp <- mean(pp.explicit$lextale, tr=.2); mean.lextale.exp
mean.lextale.inc <- mean(pp.incidental$lextale, tr=.2); mean.lextale.inc

mean.proficiency_overall.exp <- mean(pp.explicit$proficiency_overall, tr=.2); mean.proficiency_overall.exp
mean.proficiency_overall.inc <- mean(pp.incidental$proficiency_overall, tr=.2); mean.proficiency_overall.inc

mean.proficiency_oral.exp <- mean(pp.explicit$proficiency_oral, tr=.2); mean.proficiency_oral.exp
mean.proficiency_oral.inc <- mean(pp.incidental$proficiency_oral, tr=.2); mean.proficiency_oral.inc

mean.proficiency_written.exp <- mean(pp.explicit$proficiency_written, tr=.2); mean.proficiency_written.exp
mean.proficiency_written.inc <- mean(pp.incidental$proficiency_written, tr=.2); mean.proficiency_written.inc

mean.proficiency_average.exp <- mean(pp.explicit$proficiency_average, tr=.2); mean.proficiency_average.exp
mean.proficiency_average.inc <- mean(pp.incidental$proficiency_average, tr=.2); mean.proficiency_average.inc

mean.comprehension_oral.exp <- mean(pp.explicit$comprehension_oral, tr=.2); mean.comprehension_oral.exp
mean.comprehension_oral.inc <- mean(pp.incidental$comprehension_oral, tr=.2); mean.comprehension_oral.inc

mean.comprehension_written.exp <- mean(pp.explicit$comprehension_written, tr=.2); mean.comprehension_written.exp
mean.comprehension_written.inc <- mean(pp.incidental$comprehension_written, tr=.2); mean.comprehension_written.inc

mean.PRE_CRIT.exp <- mean(pp.explicit$PRE_CRIT, tr=.2); mean.PRE_CRIT.exp
mean.PRE_CRIT.inc <- mean(pp.incidental$PRE_CRIT, tr=.2); mean.PRE_CRIT.inc

mean.PRE_CONTR.exp <- mean(pp.explicit$PRE_CONTR, tr=.2); mean.PRE_CONTR.exp
mean.PRE_CONTR.inc <- mean(pp.incidental$PRE_CONTR, tr=.2); mean.PRE_CONTR.inc

mean.usage.exp <- mean(pp.explicit$usage, tr=.2); mean.usage.exp
mean.usage.inc <- mean(pp.incidental$usage, tr=.2); mean.usage.inc

trimmed.means <- cbind(mean.lextale.exp, mean.lextale.inc,
                       mean.proficiency_overall.exp, mean.proficiency_overall.inc,
                       mean.proficiency_oral.exp, mean.proficiency_oral.inc,
                       mean.proficiency_written.exp, mean.proficiency_written.inc,
                       mean.proficiency_average.exp, mean.proficiency_average.inc,
                       mean.comprehension_oral.exp, mean.comprehension_oral.inc,
                       mean.comprehension_written.exp, mean.comprehension_written.inc,
                       mean.PRE_CRIT.exp, mean.PRE_CRIT.inc,
                       mean.PRE_CONTR.exp, mean.PRE_CONTR.inc,
                       mean.usage.exp, mean.usage.inc)

write.csv(trimmed.means, file="Trimmed_means.csv")

## NON-PARAMETRIC ALTERNATIVE: WILCOXON RANK-SUM TEST = MANN-WHITNEY U ##
#########################################################################

# Wilcoxon rank-sum test; the only trouble is that it isn't easy to get a good effect size
# lextale
wilcox.lextale <- wilcox.test(lextale~learningtype, data=pp.explicit.incidental, paired=FALSE)
# proficiency_overall
wilcox.proficiency_overall <- wilcox.test(proficiency_overall~learningtype, data=pp.explicit.incidental, paired=FALSE)
# proficiency_oral
wilcox.proficiency_oral <- wilcox.test(proficiency_oral~learningtype, data=pp.explicit.incidental, paired=FALSE)
# proficiency_written
wilcox.proficiency_written <- wilcox.test(proficiency_written~learningtype, data=pp.explicit.incidental, paired=FALSE)
# proficiency_average
wilcox.proficiency_average <- wilcox.test(proficiency_average~learningtype, data=pp.explicit.incidental, paired=FALSE)
# comprehension_oral
wilcox.comprehension_oral <- wilcox.test(comprehension_oral~learningtype, data=pp.explicit.incidental, paired=FALSE)
# comprehension_written
wilcox.comprehension_written <- wilcox.test(comprehension_written~learningtype, data=pp.explicit.incidental, paired=FALSE)
# PRE_CRIT
wilcox.PRE_CRIT <- wilcox.test(PRE_CRIT~learningtype, data=pp.explicit.incidental, paired=FALSE)
# PRE_CONTR
wilcox.PRE_CONTR <- wilcox.test(PRE_CONTR~learningtype, data=pp.explicit.incidental, paired=FALSE)
# usage
wilcox.usage <- wilcox.test(usage~learningtype, data=pp.explicit.incidental, paired=FALSE)


wilcox.results<-cbind(wilcox.lextale,wilcox.proficiency_overall,wilcox.proficiency_oral,wilcox.proficiency_written,
                      wilcox.proficiency_average,wilcox.comprehension_oral,wilcox.comprehension_written,
                      wilcox.PRE_CRIT,wilcox.PRE_CONTR,wilcox.usage)

write.csv(wilcox.results, file="Wilcox_tests.csv")

# Effect size for Wilcox:
# The estimator that corresponds to the Wilcoxon test is the Hodges-Lehmann estimator;
# it's returned by wilcox.test using the conf.int=TRUE option, under "difference in location".




### COMPARE THE 3 GROUPS: EXP, INC, UNA ###
###########################################

## NON-PARAMETRIC TEST: KRUSKAL-WALLIS ##
#########################################

kruskal.lextale <- kruskal.test(lextale~learningtype, data=participantsdata); kruskal.lextale # do the test

# Interpretation:
# a) to interpret the data, look at the mean ranks:
participantsdata$ranks.lextale <- rank(participantsdata$lextale) # add rank variable for lextale
by(participantsdata$ranks.lextale, participantsdata$learningtype, mean) # get mean rank for each group

# b) you could also look at boxplots of the groups for interpretation

# c) Use POST-HOC tests
library(splancs)
library(pgirmess)

# XXXXXX END OF SCRIPT










# S T U F F ...



# lextale
wilcox.lextale <- wilcox.test(lextale~learningtype, data=pp.explicit.incidental, paired=FALSE)
wilcox.lextale

library(coin)
wilcoxsign_test(lextale~learningtype, data=pp.explicit.incidental, distribution="exact")

z <- qnorm(wilcox.lextale$p.value/2)
z

# lextale
# proficiency_overall
# proficiency_oral
# proficiency_written
# proficiency_average
# comprehension_oral
# comprehension_written
# PRE_CRIT
# PRE_CONTR


# Effect size
r.wilcox <- function(lextale.wilcox, N){
  z <- qnorm(lextale.wilcox$p.value/2)
  r <- z/sqrt(N)
  cat(lextale.wilcox$pp.explicit.incidental, "Effect size, r =", r) # calculation: r = z / sqrt(N)
}

r.wilcox(lextale.wilcox, 42)




# EFFECT SIZE

## Calculate effect size: [this only works with the simple t-test] ##
#####################################################################
ind.t.test <- t.test(lextale~learningtype, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=pp.explicit.incidental)
ind.t.test

t <- ind.t.test$statistic[[1]]
df <- ind.t.test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
r
round(r, 2)







# MORE PLOTS

library(ggplot2)
# Boxplot
boxplot.lextale <- ggplot(participantsdata, aes(learningtype, lextale))
boxplot.lextale + geom_boxplot() + labs(x = "Group", y = "Lextale")

# Density plot
density.lextale.exp <- ggplot(pp.explicit, aes(lextale)) + geom_density() + labs(x= "Lextale", y = "Density"); density.lextale.exp
density.lextale.inc <- ggplot(pp.incidental, aes(lextale)) + geom_density() + labs(x= "Lextale", y = "Density"); density.lextale.inc
density.lextale.una <- ggplot(pp.unaware, aes(lextale)) + geom_density() + labs(x= "Lextale", y = "Density"); density.lextale.una

# Historgram
histogram.lextale.exp <- ggplot(pp.explicit, aes(lextale)) + geom_histogram(colour="black", fill="#31ADA5", binwidth = 5) + labs(x="Lextale", y = "Density") + theme(legend.position = "none")
histogram.lextale.exp
histogram.lextale.inc <- ggplot(pp.incidental, aes(lextale)) + geom_histogram(colour="black", fill="#31ADA5", binwidth = 5) + labs(x="Lextale", y = "Density") + theme(legend.position = "none")
histogram.lextale.inc

# Histogram with normality curve (from https://www.statmethods.net/graphs/density.html)
hist(pp.explicit$lextale, breaks=12, col="red")
x <- pp.explicit$lextale
h <- hist(x, breaks=10, col="red", xlab="Lextale", main="Histogram with Normal Curve")
xfit <- seq(min(x), max(x), length=40)
yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

# Scatterplot
scatter.lextale <- ggplot(participantsdata, aes(lextale, learningtype)) + geom_point() + geom_smooth(method = "lm", colour = "red") + labs(x="Lextale", y = "Group")
scatter.lextale

# Kernel Density plot
d <- density(pp.explicit$lextale)
plot(d)


## Compare groups via Kernal Density ##
#######################################

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





