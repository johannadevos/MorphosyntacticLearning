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


# Explore different random-effects (RE) structures following Bates et al. (2015)
# First, find the number of dimensions in the RE structure that are supported by the data

# Define rePCA function (Bates, 2015, retrieved from https://github.com/dmbates/RePsychLing/blob/master/R/rePCA.R)
rePCA <- function(x) UseMethod('rePCA')

rePCA.merMod <- function(x) {
  chfs <- getME(x,"Tlist") # list of lower Cholesky factors
  nms <- names(chfs)
  unms <- unique(nms)
  names(unms) <- unms
  svals <- function(m) {
    vv <- svd(m,nv=0L)
    names(vv) <- c("sdev","rotation")
    vv$center <- FALSE
    vv$scale <- FALSE
    class(vv) <- "prcomp"
    vv
  }
  structure(lapply(unms,function(m) svals(bdiag(chfs[which(nms == m)]))),
            class="prcomplist")
}

summary.prcomplist <- function(object,...) {
  lapply(object,summary)
}

# Explore random-effects structure based on Bates et al. (2015)

# Random slope of learningtype over item
tic(); learningtype_item <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + learningtype*verbtype + (1 + learningtype|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(verb_learn, learningtype_item)
# ANOVA shows that model fit has decreased (higher AIC)

# Random slope of verbtype over participant
tic(); verbtype_part <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + learningtype*verbtype + (1|item) + (1+verbtype|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(verbtype_part)
anova(verb_learn, verbtype_part)
# Model fit increases substantially
# Is the number of parameters still supported by the model?
summary(rePCA(verbtype_part)) # All dimensions are supported by the data

# Random slope of verbtype over item
tic(); verbtype_item <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + learningtype*verbtype + (1+verbtype|item) + (1+verbtype|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
# "Model is nearly unidentifiable: large eigenvalue ratio" --> we won't use this model

# Random slope of testmoment over participant
tic(); testmoment_part <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + learningtype*verbtype + (1|item) + (1+verbtype+testmoment|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(verbtype_part, testmoment_part)
# ANOVA shows that model fit has decreased (higher AIC)

# Random slope of testmoment over item
tic(); testmoment_item <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + learningtype*verbtype + (1+testmoment|item) + (1+verbtype|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(verbtype_part, testmoment_item) # Marginal improvement
summary(rePCA(testmoment_item)) # Third dimension over items is not supported by the model


# Final model

tic(); final <- glmer(bin_score ~ input*testmoment*learningtype + input*testmoment*verbtype + learningtype*verbtype + (1|item) + (1+verbtype|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(final)