# Copyright Johanna de Vos (2018)
# Copyright Eva Koch (2018)

# This version was created in June 2018.
# In this version of the script, only test moments T1 and T2 are included, not T3 (which refers to a different task).
# Only the 'explicit' and 'incidental' learner groups are kept; the 'unaware' group is to small (n=6) to integrate in the analysis.
# Further, only critical items are included, not control items.

# Libraries
library(lme4); library(tictoc); library(arm)

# Set working directory to source file location (from https://eranraviv.com/r-tips-and-tricks-working-directory/)
library(rstudioapi)
set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path # get the path of your current open file
  setwd(dirname(current_path )) # set the working directory to the source file location
  print( getwd() ) # display the directory
}
set_wd()

# Clear workspace
rm(list=ls())

# Read in data
data <- read.csv("../data/Data_long.csv")
str(data)

# Remove unaware participants
data <- data[data$learningtype!="unaware",]

# Remove observations if items are not known
data <- data[data$known!="NT",]

# Remove all T3 observations
data <- data[data$testmoment!="T3",]

# Remove observations for control items
data <- data[data$verbtype!="control",]

# Drop unused factor levels (method: turning the factor into a factor again will drop the unused levels)
data$learningtype <- factor(data$learningtype) # drop 'unaware' from learningtype
data$testmoment <- factor(data$testmoment) # drop 'T3' from testmoment

# Define function to calculate probability from logit
logit2per = function(X){return(exp(X)/(1+exp(X)))}

# Alternatively, load the faraway package, with the ilogit function (returns the same result)
library(faraway)


## Model explorations

# Model with all possible fixed-effects (i.e., a three-way interaction)
tic(); start <- glmer(bin_score ~ input*testmoment*learningtype + (1|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(start)

# Does the start model fit the data significantly better as compared to a model without random intercepts over items?
tic(); start_min_item_intercepts <- glmer(bin_score ~ input*testmoment*learningtype + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(start_min_item_intercepts, start) # Start model is significantly better (p < .001); AIC difference of +- 240

# Does the start model fit the data significantly better as compared to a model without random intercepts over participants?
tic(); start_min_part_intercepts <- glmer(bin_score ~ input*testmoment*learningtype + (1|item), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(start_min_part_intercepts, start) # Start model is significantly better (p < .001); AIC difference of +- 625


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
tic(); learningtype_item <- glmer(bin_score ~ input*testmoment*learningtype + (1 + learningtype|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(start, learningtype_item) # No significant improvement, and even a slightly higher AIC. >> DISCARD

# Random slope of testmoment over participant
tic(); testmoment_part <- glmer(bin_score ~ input*testmoment*learningtype + (1|item) + (1+testmoment|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(start, testmoment_part) # No significant improvement, and even a slightly higher AIC. >> DISCARD

# Random slope of testmoment over item
tic(); testmoment_item <- glmer(bin_score ~ input*testmoment*learningtype + (1+testmoment|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(start, testmoment_item) # No significant improvement, and even a slightly higher AIC. >> DISCARD

# Random slope of input over participant
tic(); input_part <- glmer(bin_score ~ input*testmoment*learningtype + (1|item) + (1+input|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(start, input_part) # No significant improvement. >> DISCARD

# Random slope of input over item
tic(); input_item <- glmer(bin_score ~ input*testmoment*learningtype + (1+input|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(start, input_item) # Significant improvement (p = .03); small AIC difference (3).
summary(rePCA(input_item)) # All dimensions are supported by the data. >> KEEP


## Exploration of additional value of random slopes for interaction effects over items/participants

# input:testmoment participants
tic(); input_testmoment_part <- glmer(bin_score ~ input*testmoment*learningtype + (1+input|item) + (1+input:testmoment|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
# The model failed to converge. >> DISCARD

# input:testmoment items
tic(); input_testmoment_item <- glmer(bin_score ~ input*testmoment*learningtype + (1+input+input:testmoment|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(input_item, input_testmoment_item) # Significant improvement (p = .035); very small AIC difference (1).
summary(rePCA(input_testmoment_item)) # Third dimension over items is not supported by the model. >> DISCARD

# learningtype:input items
tic(); learningtype_input_item <- glmer(bin_score ~ input*testmoment*learningtype + (1+input+input:learningtype|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(input_item, learningtype_input_item) # No significant improvement. >> DISCARD

# learningtype:testmoment items
tic(); learningtype_testmoment_item <- glmer(bin_score ~ input*testmoment*learningtype + (1+input+learningtype:testmoment|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
# The model failed to converge. >> DISCARD

# input*learningtype*testmoment items
tic(); input_learningtype_testmoment_item <- glmer(bin_score ~ input*testmoment*learningtype + (1+input+input*learningtype*testmoment|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
anova(input_item, input_learningtype_testmoment_item) # No significant improvement. >> DISCARD


## Results: obtain the summary of the final model

# Final model (= identical to input_item)
final <- input_item
summary(final)

# Obtain logit confidence intervals and the odds ratios and their CI for the final model
se_final <- sqrt(diag(vcov(final)))
(tab_final <- cbind(Est = fixef(final), LL = fixef(final) - 1.96*se_final, UL = fixef(final) + 1.96*se_final)) # CIs of the estimates
exp(tab_final) # Calculate and print odds ratios and their CIs

# Final model with 'incidental' as intercept
data$learningtype <- factor(data$learningtype, levels = c("incidental", "explicit"))
final_inc <- glmer(bin_score ~ input*testmoment*learningtype + (1+input|item) + (1|participant), family = 'binomial', data = data, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); toc()
summary(final_inc)

# Obtain logit confidence intervals and the odds ratios and their CI for the final_inc model
se_final_inc <- sqrt(diag(vcov(final_inc)))
(tab_final_inc <- cbind(Est = fixef(final_inc), LL = fixef(final_inc) - 1.96*se_final_inc, UL = fixef(final_inc) + 1.96*se_final_inc)) # CIs
exp(tab_final_inc) # Calculate and print odds ratios


## Investigate model fit

# Inspect residuals with a binned residual plot
residualsplot <- binnedplot(fitted(final), resid(final, type = "response"), cex.pts=1, col.int="black", xlab = "Estimated score (as probability)")
# fitted(final) is identical to logit2per(predict(final))
# Thus, 'fitted' gives probabilities, while 'predict' gives logit values

# Interpretation
sqrt(2560) # square root of number of observations reflects the number of bins: 50.6
# Strictly counting, it looks like 5 out of the 51 bins fall outside of the 2SE bound; this is 9%,
# which is more than the expected 5%.
# There seems to be the following tendency: the lower scores (< 0.5) are predicted higher by the model
# than the observed values; higher scores (> 0.5) are predicted lower than the observed values.

# Save the plot as png
plotwidth = 8; plotheight = 6.5
PlotRecord<-recordPlot() # Save plot information into record
png("../results/Residuals_plot.png", width=plotwidth, height=plotheight,units="in",res=300) # Create png with width, height and resolution
replayPlot(PlotRecord)
dev.off()
