# Libraries
library(lme4); library(tictoc)

# Clear workspace
rm(list=ls())

# Read in data
data <- read.csv("../data/Long_data.csv")
str(data)

# Remove unaware participants
data <- data[data$learningtype!="unaware",]

# Models
fixed <- glm(bin_score ~ learningtype + verbtype + input + testmoment + input:testmoment, family = 'binomial', data = data)
summary(fixed)

# First mixed-effects model with random intercepts over participants
tic(); mixed1 <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(mixed)

# Adding random intercepts over items
tic(); mixed2 <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + (1|participant) + (1|item), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()

# Have the random intercepts over items significantly increased model fit?
anova(mixed, mixed2)

# Now, turn things around
# First mixed-effects model with random intercepts over items
tic(); mixed3 <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + (1|item), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(mixed)

# Adding random intercepts over participants
tic(); mixed4 <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + (1|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(mixed4)

# Have the random intercepts over participants significantly increased model fit?
anova(mixed3, mixed4)

# Lower AIC, BIC and deviance indicates a better fit of the model to the data
# If Pr(>Chisq) is below .05, the second model fits the data significantly better than the first

# Does including proficiency in the model lead to a better fit of the model to the data?
tic(); mixed5 <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + lextale + (1|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(mixed5)

# Have the LexTALE scores significantly increased model fit?
anova(mixed4, mixed5)

