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

# Relevel
data$testmoment <- factor(data$testmoment, levels = c("T2", "T3", "T1"))

# Models
fixed <- glm(bin_score ~ learningtype + verbtype + input + testmoment + input:testmoment, family = 'binomial', data = data)
summary(fixed)

# Mixed-effects model without interactions
tic(); mixed0 <- glmer(bin_score ~ input + testmoment + learningtype + verbtype + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(mixed0)

# First mixed-effects model with random intercepts over participants
tic(); mixed1 <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(mixed1)
# 23.95 sec elapsed

# Adding random intercepts over items
tic(); mixed2 <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + (1|participant) + (1|item), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
# 44.21 sec elapsed

# Have the random intercepts over items significantly increased model fit?
anova(mixed1, mixed2)

# Now, turn things around
# First mixed-effects model with random intercepts over items
tic(); mixed3 <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + (1|item), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(mixed3)
# 37.77 sec elapsed

# Adding random intercepts over participants
tic(); mixed4 <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + (1|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(mixed4)
# 44.08 sec elapsed

# Have the random intercepts over participants significantly increased model fit?
anova(mixed3, mixed4)

# Lower AIC, BIC and deviance indicates a better fit of the model to the data
# If Pr(>Chisq) is below .05, the second model fits the data significantly better than the first

# Does including proficiency (LexTALE) in the model lead to a better fit of the model to the data?
tic(); mixed5 <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + lextale + (1|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
# Outcome: model does not converge

# Does including proficiency (proficiency_average) in the model lead to a better fit of the model to the data?
tic(); mixed6 <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + proficiency_average + (1|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(mixed6)
# 48.58 sec elapsed

# Have the LexTALE scores significantly increased model fit?
anova(mixed4, mixed6)