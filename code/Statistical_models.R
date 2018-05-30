# Libraries
library(lme4); library(tictoc)

# Clear workspace
rm(list=ls())

# Read in data
data <- read.csv("../data/Long_data.csv")
str(data)

# Remove unaware participants
data <- data[data$learningtype!="unaware",]

# Define function to calculate probability from logit
logit2per = function(X){return(exp(X)/(1+exp(X)))}
logit2per(1.76317) # this is the probability predicted for those combination of factor levels that is represented by the intercept


# Model explorations

# Relevel
data$testmoment <- factor(data$testmoment, levels = c("T2", "T3", "T1"))
data$verbtype <- factor(data$verbtype, levels = c("critical", "control"))

# Model based on research questions
tic(); start <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + (1|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(start)

# Does the start model fit the data significantly better as compared to a model without random intercepts over items?
tic(); start_min_item_intercepts <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(start_min_item_intercepts, start)

# Does the start model fit the data significantly better as compared to a model without random intercepts over participants?
tic(); start_min_part_intercepts <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + (1|item), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(start_min_part_intercepts, start)

# Does the addition of extra fixed and/or random effects improve model fit?

# Add interaction verbtype:learningtype
tic(); verb_learn <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + learningtype*verbtype + (1|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(verb_learn)
anova(start, verb_learn)