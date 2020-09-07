################################################################################
#                                  Preparation                                 #
################################################################################

####Setup####
dev.off()
#Set the working directory to this files location:
#setwd()
rm(list = ls(all = TRUE))

####Libraries####
#install.packages("tidyverse")
library(tidyverse)
#install.packages("stringr")
library(stringr)
#install.packages("car")
library(car)
#install.packages("lme4")
library(lme4)
#install.packages("lmerTest")
library(lmerTest)
#install.packages("readxl")
library(readxl)
#https://github.com/rmcelreath/rethinking
library(rethinking)

####contrast####
options(contrasts = c("contr.helmert","contr.poly"))

####Set a seed####
set.seed(17082020)

####Getting the data####
data <- read.csv("control_analysis_data_payoffs.csv")

################################################################################
#                                Making factors                                #
################################################################################

data$counterbalancing <- factor(data$counterbalancing, 
                                         levels = c(0, 1), 
                                         labels = c("A", "B"))

data$cooperation <- factor(data$cooperation_N, 
                            levels = c(0, 1), 
                            labels = c("defection", "cooperation"))

data$cooperation_other <- factor(data$cooperation_other, 
                                         levels = c(0, 1), 
                                         labels = c("defection", "cooperation"))

################################################################################
#                                     Summary                                  #
################################################################################

################
# Demographics #
################

###Gender###
table(data$gender_match)
table(data$gender_reported)
table(data$gender_prolific)

###Age###
table(data$age_match)
table(data$age_reported)
table(data$age_prolific)

###################
# Econ Experience #
###################

#How many knew the name of the game
sum(data$know_name_game)

#Distribution of game names
table(data$name_game)

#economic experience
mEcon <- mean(data$experience_econ_num)
sdEcon <- sd(data$experience_econ_num)

#Distribution of economic experience
table(data$experience_econ_str)

#Plotting economic experience
statsText <- paste("M = ", round(mEcon, digits = 2), " (SD = ", round(sdEcon, digits = 2), ")", sep = "")
labelsText <- c("Never (1)", "Once (2)", "A few times (3)", "Often (4)", "Very Often (5)")

ggplot(data, aes(x=experience_econ_num)) +
  geom_histogram(binwidth=1, color="black", fill="#ededed") +
  geom_vline(xintercept = mEcon, linetype="dashed") +
  geom_text(x=3.5, y=13, label=statsText, size=6) + 
  scale_x_continuous(breaks = 1:5,
                     labels = labelsText,
                     name = "Experience With Economic Games") +
  ylab("Count") +
  coord_cartesian(ylim = c(0,15)) +
  theme_minimal() 

ggsave(filename = "graphs/control_econ_experience.png",
       plot = last_plot(),
       width = 6,
       height = 3.708,
       dpi = 1200)

#People truly naive 
#Reported no experience
#Did not know the name of the game
nrow(subset(data, experience_econ_num==1 & know_name_game==0))

################################################################################
#                             Cooperation Rates                                #
################################################################################

#Cooperation rate on first choice
data %>%
  count(cooperation) %>%
  mutate(prop = prop.table(n), per = paste0(round(100 * n/sum(n), 2), "%"))


################################################################################
#                             Main logistic model                              #
################################################################################

###############
# Frequentist #
###############

####Making the models####

#Intercept only#
F.MainLM.intercept <- glm(cooperation ~ 1, 
                          data = data, family = "binomial")

#Counterbalancing#
F.MainLM.counterbalancing <- glm(cooperation ~ counterbalancing, 
                                 data = data, family = "binomial")

####Model comparison####

#Model comparison is based on:
# - "Data Analysis: A model Comparison Approach to regression, ANOVA, and Beyond" by Judd et al. (2017)
# - https://github.com/StatQuest/logistic_regression_demo/blob/master/logistic_regression_demo.R

#Deviance is -2Log(likelihood)
#or -2*log((p^k)*((1-p)^(N-k))) 
#where k is the number of successes and N the total number of events

#Difference in deviance: DEV(C)-DEV(A)
#Proportional reduction in deviance (PRD) is DEV(C)-DEV(A)/DEV(C)

# Which is equivalent to McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
# where ll.null <- model$null.deviance/-2
# ll.proposed <- model$deviance/-2

#Compare models using a Chi^2 test:
#You Chi^2 value is the difference in deviance DEV(C)-DEV(A)
#or 2*(ll.proposed - ll.null).
#Get the critical Chi^2 value with qchisq(1-.05, df = df).
#Get the p value with pchisq(x, df=df, lower.tail=FALSE) where x is your Chi^2

#Custom function to compare to logistic models and get PRD and p
logisticModelCompare <- function (model1, model2){
  if(model1$deviance >= model2$deviance) {
    mC <- model1
    mA <- model2
  } else {
    mC <- model2
    mA <- model1
  }
  print(model1$deviance >= model2$deviance)
  
  df <- length(mA$coefficients) - length(mC$coefficients)
  devC <- mC$deviance
  devA <- mA$deviance
  diffDev <- devC - devA
  PRD <- diffDev / devC
  
  critChi <- qchisq(1-.05, df = df)
  p <- pchisq(diffDev, df=df, lower.tail=FALSE)
  
  outputTable <- data.frame(
    PRD=round(PRD, digits = 3),
    diffDev=round(diffDev, digits = 2),
    df=df,
    critChi=round(critChi, digits = 2),
    p=round(p, digits = 3)
  )
  
  return(
    list(
      modelC = mC$formula,
      modelA = mA$formula,
      outputTable = outputTable
    )
  )
}


#Comparing intercept model to b0 = 0 model
#p of cooperation is 0.5
p0 <- 0.5
#Get the -2 log(L) and deviance
k0 <- 15 #number of events
N0 <- 30 #number of observations
dev0 <- -2*(log((p0^k0)*((1-p0)^(N0-k0)))) 
devDiff00 <- dev0 - F.MainLM.intercept$deviance 
PRD0 <- devDiff00/dev0 
p00 <- pchisq(devDiff00, df=1, lower.tail=FALSE) 

#Compare intercept and counterbalancing
logisticModelCompare(F.MainLM.intercept, F.MainLM.counterbalancing)


####Analysing the intercept model####

#summary
summary(F.MainLM.intercept)
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)  
# (Intercept)   0.9163     0.4183    2.19   0.0285 *

#getting the odds
exp(0.9163     ) 

#getting the predicted cooperation rate which is very close to the actual rate
inv_logit(0.9163     )

#Profile confidence intervals (best)
confint(F.MainLM.intercept)

################################################################################
#                             Beliefs about others                            #
################################################################################

mean(data$prediction_other_cooperate)
sd(data$prediction_other_cooperate)

mean(data$prediction_other_cooperate_if_saw)
sd(data$prediction_other_cooperate_if_saw)

######################
# Preparing the data #
######################

#To test whether there is a difference between predictions that the other 
#cooperates, I need to change these questions from a wide to long format.

#Prepare the elements for the wide format
id <- rep(NA, nrow(data)*2)
predicted_Cooperation <- rep(NA, nrow(data)*2)
vision_condition <- rep(NA, nrow(data)*2)
choice <- rep(NA, nrow(data)*2)
movement_prediction <- rep(NA, nrow(data)*2)
index <- 0

#Populate the elements according the the looping and to the information in 
#analysis_data
for (i in 1:nrow(data)) {
  #i serves as an index of the current participant in analysis_data
  for (ii in 1:2) {
    #ii servers as an index of which of the two observations of the participant
    #we are entering
    
    index <- index + 1 #get an index of the current observation
    
    #get the id
    id[index] <- data$ID[i]
    #set one of the two conditions
    vision_condition[index] <- ii 
    #get whether they cooperated on their first choice
    choice[index] <- data$cooperation[i]
    
    if(data$prediction_other_cooperate[i] == data$prediction_other_cooperate_if_saw[i]){
      movement_prediction[index] <- 2 #equal between the two conditions
    } else if(data$prediction_other_cooperate[i] > data$prediction_other_cooperate_if_saw[i]){
      movement_prediction[index] <- 1 #decrease between the two conditions
    } else {
      movement_prediction[index] <- 3 #increase between the two conditions
    }
    
    if(ii == 1) {
      #If this is the first condition, get the prediction that the other will
      #cooperate (no_see)
      predicted_Cooperation[index] <- 
        data$prediction_other_cooperate[i]
    } else if (ii == 2) {
      #If this is the second condition, get the prediction that the other will
      #cooperate if they could see the participant's choice (see)
      predicted_Cooperation[index] <- 
        data$prediction_other_cooperate_if_saw[i]
    } 
  }
}

#Create the dataframe
visionData <- as.data.frame(cbind(
  id, 
  vision_condition,
  choice,
  predicted_Cooperation,
  movement_prediction))

#Set the variables into factors
visionData$vision_conditionN <- visionData$vision_condition
visionData$vision_condition <- 
  factor(visionData$vision_condition, levels = c(1, 2), 
         labels = c("General", "Saw Choice Beforehand"))

visionData$choiceN <- visionData$choice
visionData$choice <- 
  factor(visionData$choice, levels = c(1, 2), 
         labels = c("Defection", "Cooperation"))

visionData$movement_prediction <- 
  factor(visionData$movement_prediction, levels = c(1, 2, 3), 
         labels = c("decrease", "equal", "increase"))

####################
# Model Comparison #
####################

ml.1 <- lmer(predicted_Cooperation~1+vision_condition + (1|id), 
             data = visionData, REML = F)

ml.2 <- lmer(predicted_Cooperation~1+vision_condition+choice + (1|id), 
             data = visionData, REML = F)

ml.3 <- lmer(predicted_Cooperation~1+vision_condition*choice + (1|id), 
             data = visionData, REML = F)

anova(ml.1, ml.2, ml.3)
anova(ml.2, ml.3)
anova(ml.1, ml.2)
#Model 2 is the best model

#####################################
# Fixed effects and interpretations #
#####################################

reml.3 <- lmer(predicted_Cooperation~1+vision_condition*choice + (1|id), 
               data = visionData, REML = T)
summary(reml.3)
confint(reml.3)

#write.csv(visionData, file="control_vision_data_all.csv", row.names = F)

aggregate(predicted_Cooperation ~ vision_condition*choice, FUN = mean, data = visionData)
aggregate(predicted_Cooperation ~ vision_condition*choice, FUN = sd, data = visionData)

aggregate(predicted_Cooperation ~ vision_condition, FUN = mean, data = visionData)
aggregate(predicted_Cooperation ~ vision_condition, FUN = sd, data = visionData)

aggregate(predicted_Cooperation ~ choice, FUN = mean, data = visionData)
aggregate(predicted_Cooperation ~ choice, FUN = sd, data = visionData)


nrow(
  visionData %>%
    subset(choice != "Cooperation" & vision_condition != "Saw Choice Beforehand" & predicted_Cooperation > 0.50)
) 
nrow(visionData %>%
       subset(choice != "Cooperation"))/2

nrow(
  visionData %>%
    subset(choice == "Cooperation" & vision_condition == "Saw Choice Beforehand" & predicted_Cooperation > 0.50)
) 
nrow(visionData %>%
       subset(choice == "Cooperation"))/2

#######################
# Testing Assumptions #
#######################

mod.residuals <- residuals(reml.3)
mod.predictors <- predict(reml.3)

dev.off()
png("graphs/control_belief_assumptions.png", height = 12.361, width = 20, res = 300, units = 'cm')
par(mfrow=c(1,2), mar = c(5,4,2,1))
plot(mod.predictors, mod.residuals, 
     ylab = "Residuals",
     xlab = "Predictions",
     main = "(a)")
abline(h=mean(mod.residuals))

qqnorm(mod.residuals, main = "(b)")
qqline(mod.residuals)
dev.off()

cooksd = cooks.distance(reml.3)
cookOut = which(cooksd >= 1)

##########
# Graphs #
##########

ggplot(visionData, aes(x = vision_condition, y=predicted_Cooperation, 
                       group=movement_prediction,colour=movement_prediction)) +
  geom_violin(trim=T, inherit.aes = F, aes(x = vision_condition, y=predicted_Cooperation), fill="#ededed") +
  geom_boxplot(width=0.1, inherit.aes = F, aes(x = vision_condition, y=predicted_Cooperation)) +
  geom_line(aes(x = vision_condition, y=predicted_Cooperation, group = factor(id))) +
  stat_summary(aes(x = vision_condition, y=predicted_Cooperation, group=choice),
               fun = mean, geom="line", size = 1, colour = "black") +
  stat_summary(aes(x = vision_condition, y=predicted_Cooperation, group=choice),
               fun = mean, geom="point", size = 2, colour = "black") +
  facet_wrap(~choice) + 
  scale_y_continuous(labels = scales::number_format(accuracy = .01),
                     breaks= seq(0, 1, by = .05)) +
  coord_fixed(ratio = 3/1, ylim = c(0, 1)) + 
  geom_hline(yintercept=0.5, linetype="dashed") +
  labs(x = "Conditions for Reporting Belief", y = "Belief Other Will Cooperate \n (probability)", 
       color = "Change Between Conditions \n for Reporting Belief") + 
  theme_minimal() + 
  theme(legend.position="bottom")

ggsave(filename = "graphs/control_beliefs_general.png",
       plot = last_plot(),
       width = 12,
       height = 7.416,
       dpi = 1200)

###########################
# Clusters of differences #
###########################

data$difference <- data$prediction_other_cooperate_if_saw - data$prediction_other_cooperate

ggplot(data = data, aes(y=difference, x=cooperation)) +
  geom_violin(trim=T, fill="#ededed") + 
  geom_boxplot(width=0.1) + 
  geom_dotplot(binaxis='y', 
               stackdir='center',
               binwidth = 0.001, #Make a small binwidth so that all the dots actually show
               dotsize=20, 
               alpha=0.50) + 
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x = "Participant's Choice", 
       y = "Difference in Belief Between the Two Reporting Conditions \n (probability difference)") +
  scale_x_discrete(labels=c("defection" = "Defection",
                            "cooperation" = "Cooperation"))+
  theme_minimal()

ggsave(filename = "graphs/control_beliefs_differences.png",
       plot = last_plot(),
       width = 6,
       height = 5,
       dpi = 1200)

################################################################################
#                       Comparing rate with experimental                       #
################################################################################
#Preparing data to compare with experimental group

mixedData <- data.frame(
  ID = data$ID,
  cooperation = data$cooperation_N,
  experiment = data$experiment
)

write.csv(mixedData, "compareCnE/control.csv", row.names = F)