## Copyright Eva Koch (2019)
## Copyright Johanna de Vos (2018)

## This script contains the graphs for the morphosyntactic learning study.
## The script was written by Eva Koch and is partially based on an older script of Johanna de Vos (https://github.com/johannadevos/NaturalisticL2WordLearning/blob/master/Script.R)
## The 'intentional group' is called 'explicit' in the original dataset.

# Set working directory to source file location
library(rstudioapi)
set_wd <- function() {
  library(rstudioapi) # make sure you have it installed
  current_path <- getActiveDocumentContext()$path # get the path of your current open file
  setwd(dirname(current_path )) # set the working directory to the source file location
  print( getwd() ) # display the directory
}
set_wd()


## GRAPHING: INTENTIONAL VS. INCIDENTAL GROUPS

## Data preparation

rm(list=ls()) # Clear workspace
library(ggplot2) # Load packages

# Load data
data <- read.csv("../data/Data_long.csv")

# Remove data
data <- data[data$learningtype!="unaware",] # Remove unaware participants
data <- data[data$known!="NT",] # Remove observations if items are not known
# data <- data[data$verbtype!="control",] # Only do this if you do not wish to plot the control items

# Drop unused factor levels (method: turning the factor into a factor again will drop the unused levels)
data$learningtype <- factor(data$learningtype) # drop 'unaware' from learningtype

# Reorder factor levels
input; verbtype; learningtype; testmoment # Check current order of levels
data$input <- factor(data$input, levels = c("yes", "no"), ordered=TRUE)
data$verbtype <- factor(data$verbtype, levels = c("critical", "control"), ordered=TRUE)
data$testmoment <- factor(data$testmoment, levels = c("T1", "T2", "T3"))

# Prepare the confidence interval data
dataCI <- read.csv("../data/Data_CIs.csv") # Load separate datafile with confidence interval (this will be necessary to plot the errorbars; the automatic errorbars included in ggplot2 did return CIs that were too narrow)
dataCI <- dataCI[dataCI$learningtype!="unaware",] # Remove unaware participants
data <- merge(data, dataCI, by = c('learningtype', 'verbtype', 'testmoment', 'input')) # Merge this with main data file (number of obs. should not change; the number of variables should increase by 2)


## Create graph

pd <- position_dodge(width = 0.15)
mygraph <- ggplot(data,
                  aes(data$testmoment, data$bin_score*100,
                      colour=data$verbtype, shape=data$learningtype, group=interaction(data$verbtype, data$learningtype, data$input))) +
  theme_bw() + # different style
  scale_y_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) + # Y-axis: A tick mark every 10%
  labs(x = "Test moment", y = "Accuracy percentage") + # adapt axis labels
  theme(text = element_text(size = 23), axis.text.y = element_text(size = 21), axis.text.x = element_text(size = 21), strip.text = element_text(size=23))

mygraph +
  stat_summary(fun.y=mean, geom = "point", size = 5, position = pd) + 
  stat_summary(fun.y=mean, geom = "line", aes(linetype = data$input), size = 1, position = pd) + 
#  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", alpha = 0.8,  size = 0.8, position = pd) + # Returns too narrow errorbars, probably because does not account for nested data?
#  geom_linerange(data = data, mapping=aes(x=data$testmoment, ymin = data$lower, ymax = data$upper), position = pd, size = 0.8) +
  geom_errorbar(data = data, mapping=aes(x=data$testmoment, ymin = data$lower, ymax = data$upper), position = pd, width = 0.3, size = 0.8) +
  scale_shape_discrete(name = "Group", labels = c("Intentional", "Incidental")) +
  scale_colour_manual(name = "Item type", labels = c("Critical", "Control"), values = c("navyblue", "orange1")) +
  scale_linetype_discrete(name = "Input", labels = c("Yes", "No")) 

plotwidth = 8
plotheight = 6.5
ggsave("../plots/plot_T3_maingroups.png", width=plotwidth, height=plotheight, units="in", dpi = 320) # Save graph



## GRAPHING: UNAWARE SUBGROUP

## Data preparation

rm(list=ls()) # Clear workspace
library(ggplot2) # Load packages

# Load data
data <- read.csv("../data/Data_long.csv")

# Remove data
data <- data[data$learningtype!="explicit",] # Remove explicit participants
data <- data[data$learningtype!="incidental",] # Remove incidental participants
data <- data[data$known!="NT",] # Remove observations if items are not known
# data <- data[data$verbtype!="control",] # Only do this if you do not wish to plot the control items

# Drop unused factor levels (method: turning the factor into a factor again will drop the unused levels)
data$learningtype <- factor(data$learningtype) # drop explicit and incidental from learningtype

# Reorder factor levels
input; verbtype; learningtype; testmoment # Check current order of levels
data$input <- factor(data$input, levels = c("yes", "no"), ordered=TRUE)
data$verbtype <- factor(data$verbtype, levels = c("critical", "control"), ordered=TRUE)

# Prepare the confidence interval data
dataCI <- read.csv("../data/Data_CIs.csv") # Load separate datafile with confidence interval (this will be necessary to plot the errorbars; the automatic errorbars included in ggplot2 did return CIs that were too narrow)
dataCI <- dataCI[dataCI$learningtype!="explicit",] # Remove unaware participants
dataCI <- dataCI[dataCI$learningtype!="incidental",] # Remove incidental participants
data <- merge(data, dataCI, by = c('learningtype', 'verbtype', 'testmoment', 'input')) # Merge this with main data file (number of obs. should not change; the number of variables should increase by 2)


## Create graph
pd <- position_dodge(width = 0.15)
mygraph <- ggplot(data,
                  aes(data$testmoment, data$bin_score*100,
                      colour=data$verbtype, group=interaction(data$verbtype, data$input))) +
  theme_bw() + # different style
  scale_y_continuous(breaks=seq(0, 100, 20), limits=c(0,100)) + # Y-axis: A tick mark every 10%
  ggtitle("Test scores: Unaware group") +
  labs(x = "Test moment", y = "Accuracy percentage") + # adapt axis labels
  theme(text = element_text(size = 23), axis.text.y = element_text(size = 21), axis.text.x = element_text(size = 21), strip.text = element_text(size=23), plot.title = element_text(hjust = 0.5, size = 23))

mygraph +
  stat_summary(fun.y=mean, geom = "point", size = 5, position = pd) + 
  stat_summary(fun.y=mean, geom = "line", aes(linetype = data$input), size = 1, position = pd) + 
  # geom_linerange(data = data, mapping=aes(x=data$testmoment, ymin = data$lower, ymax = data$upper), position = pd, size = 0.8) +
  geom_errorbar(data = data, mapping=aes(x=data$testmoment, ymin = data$lower, ymax = data$upper), position = pd, width = 0.2, size = 0.8) +
  scale_colour_manual(name = "Item type", labels = c("Critical", "Control"), values = c("navyblue", "orange1")) +
  scale_linetype_discrete(name = "Input", labels = c("Yes", "No")) 

plotwidth = 8
plotheight = 6.5
ggsave("../plots/plot_T3_unaware.png", width=plotwidth, height=plotheight, units="in", dpi = 320) # Save graph


# # End of script # #