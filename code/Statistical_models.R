# Copyright Johanna de Vos (2018)
# Copyright Eva Koch (2018)

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

# Model with all possible fixed-effects (i.e., a four-way interaction)
tic(); start <- glmer(bin_score ~ input*testmoment*learningtype*verbtype + (1|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(start)

# Does the start model fit the data significantly better as compared to a model without random intercepts over items?
tic(); start_min_item_intercepts <- glmer(bin_score ~ input*testmoment*learningtype*verbtype + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(start_min_item_intercepts, start)

# Does the start model fit the data significantly better as compared to a model without random intercepts over participants?
tic(); start_min_part_intercepts <- glmer(bin_score ~ input*testmoment*learningtype*verbtype + (1|item), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(start_min_part_intercepts, start)

# Does the addition of extra random effects improve model fit?
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
tic(); learningtype_item <- glmer(bin_score ~ input*testmoment*learningtype*verbtype + (1 + learningtype|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(start, learningtype_item) # No significant improvement, and even a higher AIC

# Random slope of verbtype over participant
tic(); verbtype_part <- glmer(bin_score ~ input*testmoment*learningtype*verbtype + (1|item) + (1+verbtype|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(start, verbtype_part) # Model fit increases substantially and significantly
summary(rePCA(verbtype_part)) # All dimensions are supported by the data

# Random slope of verbtype over item
tic(); verbtype_item <- glmer(bin_score ~ input*testmoment*learningtype*verbtype + (1+verbtype|item) + (1+verbtype|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(verbtype_part, verbtype_item) # Model fit increases significantly
summary(rePCA(verbtype_item)) # All dimensions are supported by the data

# Random slope of testmoment over participant
tic(); testmoment_part <- glmer(bin_score ~ input*testmoment*learningtype*verbtype + (1+verbtype|item) + (1+verbtype+testmoment|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(verbtype_item, testmoment_part) # No significant improvement, and even a higher AIC

# Random slope of testmoment over item
tic(); testmoment_item <- glmer(bin_score ~ input*testmoment*learningtype*verbtype + (1+verbtype+testmoment|item) + (1+verbtype|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(verbtype_item, testmoment_item) # Marginal improvement
summary(rePCA(testmoment_item)) # Third dimension over items is not supported by the model

# Final model (= identical to verbtype_item)
final <- verbtype_item
summary(final)

# Obtain confidence intervals and odds ratios for the final model
se <- sqrt(diag(vcov(final)))
(tab <- cbind(Est = fixef(final), LL = fixef(final) - 1.96*se, UL = fixef(final) + 1.96*se)) # CIs
exp(tab) # Calculate and print odds ratios


# A simpler model to investigate the main effect of condition at T1

# Relevel
data$testmoment <- factor(data$testmoment, levels = c("T1", "T2", "T3"))

# Model
tic(); simple <- glmer(bin_score ~ verbtype*learningtype + testmoment*learningtype + verbtype*testmoment + (1+verbtype|item) + (1+verbtype|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(simple)

# Obtain confidence intervals and odds ratios for the simple model
se_main <- sqrt(diag(vcov(simple)))
(tab_main <- cbind(Est = fixef(simple), LL = fixef(simple) - 1.96*se_main, UL = fixef(simple) + 1.96*se_main)) # CIs
exp(tab_main) # Calculate and print odds ratios

# Relevel to obtain insight in control items
data$verbtype <- factor(data$verbtype, levels = c("control", "critical"))
tic(); simple_contr <- glmer(bin_score ~ verbtype*learningtype + testmoment*learningtype + verbtype*testmoment + (1+verbtype|item) + (1+verbtype|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(simple_contr)