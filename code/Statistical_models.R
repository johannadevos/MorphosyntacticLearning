# Copyright Johanna de Vos (2018)
# Copyright Eva Koch (2018)

# Libraries
library(lme4); library(tictoc); library(arm)

# Clear workspace
rm(list=ls())

# Read in data
data <- read.csv("../data/Data_long.csv")
str(data)

# Remove unaware participants
data <- data[data$learningtype!="unaware",]

# Remove observations if items are not known
data <- data[data$known!="NT",]

# Define function to calculate probability from logit
logit2per = function(X){return(exp(X)/(1+exp(X)))}


## Model explorations

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


## Explore different random-effects (RE) structures following Bates et al. (2015)
## Does the addition of extra random effects improve model fit?

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

# Random slope of learningtype over item
tic(); learningtype_item <- glmer(bin_score ~ input*testmoment*learningtype*verbtype + (1 + learningtype|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(start, learningtype_item) # No significant improvement, and even a higher AIC

# Random slope of verbtype over participant
tic(); verbtype_part <- glmer(bin_score ~ input*testmoment*learningtype*verbtype + (1|item) + (1+verbtype|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(start, verbtype_part) # Model fit increases substantially and significantly
summary(rePCA(verbtype_part)) # All dimensions are supported by the data

# Random slope of verbtype over item
tic(); verbtype_item <- glmer(bin_score ~ input*testmoment*learningtype*verbtype + (1+verbtype|item) + (1+verbtype|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
# Output: Warning message 'Model failed to converge'; remove this random slope again from the model

# Random slope of testmoment over participant
tic(); testmoment_part <- glmer(bin_score ~ input*testmoment*learningtype*verbtype + (1|item) + (1+verbtype+testmoment|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(verbtype_item, testmoment_part) # No significant improvement, and even a higher AIC

# Random slope of testmoment over item
tic(); testmoment_item <- glmer(bin_score ~ input*testmoment*learningtype*verbtype + (1+testmoment|item) + (1+verbtype|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(verbtype_item, testmoment_item) # Marginal improvement
summary(rePCA(testmoment_item)) # Third dimension over items is not supported by the model

# Final model (= identical to verbtype_part)
final <- verbtype_part
summary(final)

# Obtain confidence intervals and odds ratios for the final model
se_final <- sqrt(diag(vcov(final)))
(tab_final <- cbind(Est = fixef(final), LL = fixef(final) - 1.96*se_final, UL = fixef(final) + 1.96*se_final)) # CIs
exp(tab_final) # Calculate and print odds ratios

# Final model with 'incidental' as intercept
data$learningtype <- factor(data$learningtype, levels = c("incidental", "explicit"))
final_inc <- glmer(bin_score ~ input*testmoment*learningtype*verbtype + (1|item) + (1+verbtype|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
summary(final_inc)

# Obtain confidence intervals and odds ratios for the final_inc model
se_final_inc <- sqrt(diag(vcov(final_inc)))
(tab_final_inc <- cbind(Est = fixef(final_inc), LL = fixef(final_inc) - 1.96*se_final_inc, UL = fixef(final_inc) + 1.96*se_final_inc)) # CIs
exp(tab_final_inc) # Calculate and print odds ratios

# Final model with T3 as intercept (to investigate the main effect of condition at T3, the explicit posttest)
# (while input = no input, verbtype = critical, learningtype = explicit)
data$testmoment <- factor(data$testmoment, levels = c("T3", "T2", "T1")) # Relevel testmoment
data$learningtype <- factor(data$learningtype, levels = c("explicit", "incidental")) # Relevel learningtype
final_T3 <- glmer(bin_score ~ input*testmoment*learningtype*verbtype + (1|item) + (1+verbtype|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
summary(final_T3)

# Obtain confidence intervals and odds ratios for the final_T3 model
se_final_T3 <- sqrt(diag(vcov(final_T3)))
(tab_final_T3 <- cbind(Est = fixef(final_T3), LL = fixef(final_T3) - 1.96*se_final_T3, UL = fixef(final_T3) + 1.96*se_final_T3)) # CIs
exp(tab_final_T3) # Calculate and print odds ratios


## Investigate model fit

# Inspect residuals
binnedplot(fitted(final), resid(final, type = "response"), cex.pts=1, col.int="black", xlab = "Estimated score (as probability)")
# fitted(final) is identical to logit2per(predict(final))
# Thus, 'fitted' gives probabilities, while 'predict' gives logit values


## A simpler model to investigate the main effect of condition at T1

# Relevel
data$testmoment <- factor(data$testmoment, levels = c("T1", "T2", "T3"))

# Model
tic(); simple <- glmer(bin_score ~ verbtype*learningtype + testmoment*learningtype + (1|item) + (1+verbtype|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(simple)

# Obtain confidence intervals and odds ratios for the simple model
se_simple <- sqrt(diag(vcov(simple)))
(tab_simple <- cbind(Est = fixef(simple), LL = fixef(simple) - 1.96*se_simple, UL = fixef(simple) + 1.96*se_simple)) # CIs
exp(tab_simple) # Calculate and print odds ratios

# Relevel to obtain insight in control items
data$verbtype <- factor(data$verbtype, levels = c("control", "critical"))
tic(); simple_contr <- glmer(bin_score ~ verbtype*learningtype + testmoment*learningtype + (1|item) + (1+verbtype|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(simple_contr)

# Obtain confidence intervals and odds ratios for the simple_contr model
se_simple_contr <- sqrt(diag(vcov(simple_contr)))
(tab_simple_contr <- cbind(Est = fixef(simple_contr), LL = fixef(simple_contr) - 1.96*se_simple_contr, UL = fixef(simple_contr) + 1.96*se_simple_contr)) # CIs
exp(tab_simple_contr) # Calculate and print odds ratios
