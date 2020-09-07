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
#install.packages("rstanarm")
#install.packages("logspline")
library(rstanarm)
#install.packages("bayestestR")
library(bayestestR)
#install.packages('see')
library(see)
#install.packages("bayesplot")
library(bayesplot)
#https://github.com/rmcelreath/rethinking
library(rethinking)

####contrast####
options(contrasts = c("contr.helmert","contr.poly"))

####Set a seed####
set.seed(17082020)

####Getting the data####
data <- read.csv("translucency_analysis_data_payoffs.csv")
################################################################################
#                                Making factors                                #
################################################################################

data$counterbalancing <- factor(data$counterbalancing, 
                                levels = c(0, 1), 
                                labels = c("A", "B"))

data$experimental_condition <- factor(data$experimental_condition, 
                                      levels = c(1,2,3,4), 
                                      labels = c("20%-Defect",
                                                 "20%-Symm",
                                                 "80%-Defect",
                                                 "80%-Symm"))

data$translucency_level <- factor(data$translucency_level, 
                                  levels = c(0, 1), 
                                  labels = c("20%", "80%"))

data$translucency_direction <- factor(data$translucency_direction, 
                                      levels = c(0, 1), 
                                      labels = c("asymmetric-defection", "symmetric"))

data$cooperation_initial <- factor(data$cooperation_initial_N, 
                                   levels = c(0, 1), 
                                   labels = c("defection", "cooperation"))

data$cooperation_final <- factor(data$cooperation_final_N, 
                                 levels = c(0, 1), 
                                 labels = c("defection", "cooperation"))

data$cooperation_initial_other <- factor(data$cooperation_initial_other, 
                                         levels = c(0, 1), 
                                         labels = c("defection", "cooperation"))

data$cooperation_final_other <- factor(data$cooperation_final_other, 
                                       levels = c(0, 1), 
                                       labels = c("defection", "cooperation"))

data$translucency_change <- factor(data$translucency_change, 
                                   levels = c(0, 1), 
                                   labels = c("same", "switch"))

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
  geom_text(x=3, y=50, label=statsText, size=6) + 
  scale_x_continuous(breaks = 1:5,
                     labels = labelsText,
                     name = "Experience With Economic Games") +
  ylab("Count") +
  theme_minimal() 

ggsave(filename = "graphs/translucency_econ_experience.png",
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
  count(cooperation_initial) %>%
  mutate(prop = prop.table(n), per = paste0(round(100 * n/sum(n), 2), "%"))

#Getting cooperation count and proportions
#According to the conditions:

#Experimental
for (i in 1:4) {
  condition <- levels(data$experimental_condition)[i]
  print(condition)
  
  print(
    data %>%
      subset(experimental_condition == condition) %>%
      count(cooperation_initial) %>%
      mutate(prop = prop.table(n), per = paste0(round(100 * n/sum(n), 2), "%"))
  )
}

#Direction
for (i in 1:2) {
  condition <- levels(data$translucency_direction)[i]
  print(condition)
  
  print(
    data %>%
      subset(translucency_direction == condition) %>%
      count(cooperation_initial) %>%
      mutate(prop = prop.table(n), per = paste0(round(100 * n/sum(n), 2), "%"))
  )
}

#Level
for (i in 1:2) {
  condition <- levels(data$translucency_level)[i]
  print(condition)
  
  print(
    data %>%
      subset(translucency_level == condition) %>%
      count(cooperation_initial) %>%
      mutate(prop = prop.table(n), per = paste0(round(100 * n/sum(n), 2), "%"))
  )
}

################################################################################
#                           Translucency behaviour                             #
################################################################################

#Noting whether other player's choice could be detected

#Participants could detect others either if the direction was symmetric or if
#the other player defected
data$other_couldBeDetected <- if_else(
  data$translucency_direction == "symmetric" |
    data$cooperation_initial_other == "defection",
  TRUE, FALSE)

#Select only people that could detect the other player
data_couldDetect <- subset(data, other_couldBeDetected)

#Getting general rate of detecting other if other could be detected
data_couldDetect %>%
  count(detectedOther) %>%
  mutate(prop = prop.table(n), per = paste0(round(100 * n/sum(n), 2), "%"))

#Select only people that could detect the other player and who did
data_couldNdetected <- subset(data_couldDetect, detectedOther)

#Count how many changed or sticked with their choices
data_couldNdetected %>%
  count(translucency_change) %>%
  mutate(prop = prop.table(n), per = paste0(round(100 * n/sum(n), 2), "%"))

#Proportions of people who switch when they detected the other participant
#according to the choice the participant made and the choice they made
xtabs(~cooperation_initial+translucency_change+cooperation_initial_other, data =  data_couldNdetected)

#The logistic model to test this situation
translucency_choices_LM <- glm(cooperation_final ~ cooperation_initial_other, 
                               data = data_couldNdetected, family = "binomial")
summary(translucency_choices_LM)
confint(translucency_choices_LM)

#Cooperation rate 
data_couldNdetected %>%
  subset(cooperation_initial_other == "defection") %>%
  count(cooperation_final) %>%
  mutate(prop = prop.table(n), per = paste0(round(100 * n/sum(n), 2), "%"))

data_couldNdetected %>%
  subset(cooperation_initial_other == "cooperation") %>%
  count(cooperation_final) %>%
  mutate(prop = prop.table(n), per = paste0(round(100 * n/sum(n), 2), "%"))

################################################################################
#                             Main logistic model                              #
################################################################################

##########################
# Checking the contrasts #
##########################

contrasts(data$translucency_level)
contrasts(data$translucency_direction)

###############
# Frequentist #
###############

####Making the models####

#Intercept only#
F.MainLM.intercept <- glm(cooperation_initial ~ 1, 
                          data = data, family = "binomial")

#Counterbalancing#
F.MainLM.counterbalancing <- glm(cooperation_initial ~ counterbalancing, 
                                 data = data, family = "binomial")

#Level#
F.MainLM.level <- glm(cooperation_initial ~ translucency_level,
                      data = data, family = "binomial")

#Direction#
F.MainLM.direction <- glm(cooperation_initial ~ translucency_direction,
                          data = data, family = "binomial")

#directionNlevel#
F.MainLM.directionNlevel <- glm(cooperation_initial ~ translucency_direction+translucency_level,
                                data = data, family = "binomial")

#Interaction#
F.MainLM.interaction <- glm(cooperation_initial ~ translucency_direction*translucency_level,
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
k0 <- 65 #number of events
N0 <- 130 #number of observations
dev0 <- -2*(log((p0^k0)*((1-p0)^(N0-k0)))) #180.2183
devDiff00 <- dev0 - F.MainLM.intercept$deviance #27.46354
PRD0 <- devDiff00/dev0 #0.1523904
p00 <- pchisq(devDiff00, df=1, lower.tail=FALSE) #1.600845e-07

#Compare intercept and counterbalancing
logisticModelCompare(F.MainLM.intercept, F.MainLM.counterbalancing)


#Compare intercept and interaction
logisticModelCompare(F.MainLM.intercept, F.MainLM.interaction)


#Compare intercept and directionNlevel
logisticModelCompare(F.MainLM.intercept, F.MainLM.directionNlevel)

#Compare intercept and direction
logisticModelCompare(F.MainLM.intercept, F.MainLM.direction)

#Compare intercept and level
logisticModelCompare(F.MainLM.intercept, F.MainLM.level)

####Analysing the full model####

summary(F.MainLM.interaction)
round(exp(F.MainLM.interaction$coefficients), digits = 2)
confint(F.MainLM.interaction)


#Getting the predicted probabilities
interaction.predicted <- data.frame(
  predicted=F.MainLM.interaction$fitted.values,
  direction=data$translucency_direction,
  level=data$translucency_level
)

#Getting the avg probabilities across direction
aggregate(predicted~direction, FUN=mean, data=interaction.predicted)

#Getting the avg probabilities across level
aggregate(predicted~level, FUN=mean, data=interaction.predicted)

#Getting the avg probabilities across the four experimental conditions
predict.4exp <- aggregate(predicted~direction*level, FUN=mean, data=interaction.predicted)

#Plotting the predictions
ggplot(data = predict.4exp, aes(y=predicted, x=level, group=direction, colour=direction)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::number_format(accuracy = .01),
                     breaks= seq(0, 1, by = .05)) +
  coord_fixed(ratio = 2.5/1, ylim = c(0, 1)) + 
  labs(x = "Translucency Level", y = "Predicted Cooperation Levels \n (probability)", 
       color = "Direction of Translucency") + 
  theme_minimal()


ggsave(filename = "graphs/translucency_predicted_cooperation.png",
       plot = last_plot(),
       width = 6,
       height = 3.708,
       dpi = 1200)

####Analysing the intercept model####

#summary
summary(F.MainLM.intercept)
exp(0.9491) #2.583384

#getting the predicted cooperation rate which is very close to the actual rate
inv_logit(0.9491) #0.7209341

#Profile confidence intervals (best)
confint(F.MainLM.intercept)
#       2.5 %    97.5 % 
#    0.5740405 1.3460237

#Profile CI in odds
exp(confint(F.MainLM.intercept))
#       2.5 %    97.5 % 
#   1.775426 3.842118

#Profile CI in probability
inv_logit(confint(F.MainLM.intercept))
#     2.5 %    97.5 % 
# 0.6396950 0.7934788

####Testing effect of understanding the payoffs on cooperation####

#Preparing the variable
data$understood_payoffs_factor <- factor(as.numeric(data$understood_payoffs), 
                                         levels = c(0, 1), 
                                         labels = c("no", "yes"))
contrasts(data$understood_payoffs_factor)

#Running the logistic regression
F.MainLM.understood <- glm(cooperation_initial ~ understood_payoffs_factor, 
                           data = data, family = "binomial")
summary(F.MainLM.understood)
exp(-0.4120)
confint(F.MainLM.understood)

#Getting the cooperation rates
#Understood
subset(data, understood_payoffs) %>%
  count(cooperation_initial) %>%
  mutate(prop = prop.table(n), per = paste0(round(100 * n/sum(n), 2), "%"))

#Did not understand
subset(data, !understood_payoffs) %>%
  count(cooperation_initial) %>%
  mutate(prop = prop.table(n), per = paste0(round(100 * n/sum(n), 2), "%"))

############
# Bayesian #
############

####Visualising the priors####

#Creating the priors
stPrior <- rt(1e6, df=7)*5 + 0
nPrior <- rnorm(1e6, 0, 1.5)

#Graphing the priors
dev.off()
png("graphs/priors.png", height = 9.271, width = 15, res = 300, units = 'cm')
par(mfrow=c(2,2), mar = c(3,3,2,1))
dens(stPrior, col="#00BFC4", ylim=c(0,0.3), xlim=c(-20,20), lwd=2,
     ann = F, cex.axis=0.75)
abline(v = 0, lty=2)
mtext(side = 1, text = "Parameter Estimates (logOdds)", line = 2, cex=0.5)
mtext(side = 2, text = "Probability Density", line = 2, cex=0.5)
title(main = "Student-t Prior (logOdds Scale)", cex.main=0.75)
dens(inv_logit(stPrior), col="#00BFC4", ylim=c(0,8), xlim=c(0,1), lwd=2,
     ann = F, cex.axis=0.75)
mtext(side = 1, text = "Parameter Estimates (probabilities)", line = 2, cex=0.5)
mtext(side = 2, text = "Probability Density", line = 2, cex=0.5)
abline(v = 0.5, lty=2)
title(main = "Student-t Prior (Probability Scale)", cex.main=0.75)
dens(nPrior, col="#00BFC4", ylim=c(0,0.3), xlim=c(-20,20), lwd=2,
     ann = F, cex.axis=0.75)
mtext(side = 1, text = "Parameter Estimates (logOdds)", line = 2, cex=0.5)
mtext(side = 2, text = "Probability Density", line = 2, cex=0.5)
abline(v = 0, lty=2)
title(main = "Normal Prior (logOdds Scale)", cex.main=0.75)
dens(inv_logit(nPrior), col="#00BFC4", ylim=c(0,8), xlim=c(0,1), lwd=2,
     ann = F, cex.axis=0.75)
mtext(side = 1, text = "Parameter Estimates (probabilities)", line = 2, cex=0.5)
mtext(side = 2, text = "Probability Density", line = 2, cex=0.5)
abline(v = 0.5, lty=2)
title(main = "Normal Prior (Probability Scale)", cex.main=0.75)
dev.off()

####Modelling####
#https://cran.r-project.org/web/packages/rstanarm/vignettes/rstanarm.html
#https://mc-stan.org/rstanarm/reference/as.matrix.stanreg.html

#Preparing the priors
interceptPrior <- normal(location = 0, scale = 1.5)
predictorsPrior <- student_t(df = 7, location = 0, scale = 2.5)

#Creating the model
B.MainLM.interaction <- stan_glm(
  cooperation_initial ~ translucency_direction*translucency_level,
  data = data,
  family = binomial(link = "logit"),
  prior = predictorsPrior, prior_intercept = interceptPrior, 
  chains=4, 
  seed = 17082020)

#General model info
summary(B.MainLM.interaction, digits = 3)

#Checking the traces of the model
mcmc_trace(B.MainLM.interaction)

ggsave(filename = "graphs/bayes_main_mcmc.png",
       plot = last_plot(),
       width = 6,
       height = 3.708,
       dpi = 1200)

#Detailed information
describe_posterior(
  B.MainLM.interaction,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

#Get the hdi
bayestestR::hdi(B.MainLM.interaction, ci = .89)

#Get the Bayes factors for the model parameters
BFs <- bayesfactor_parameters(B.MainLM.interaction)
BFs

#Get the posteriors from the model
posteriors <- as.data.frame(B.MainLM.interaction)
dim(posteriors)

###Plot the model###
#Prepare titles for the different plots
mainTitles <- c("Intercept",
                "Translucency Direction",
                "Translucency Level",
                "Direction x Level")
#Get the frequentist values of the parameters
freqValues <- F.MainLM.interaction$coefficients

dev.off()
png("graphs/main_prediction_bayesian.png", height = 9.271, width = 15, res = 300, units = 'cm')
par(mfrow=c(2,2), mar = c(3,3,2,1))

for (i in 1:4) {
  
  if (i == 1) {
    #give the intercept prior
    prior <- nPrior
  } else {
    #give the parameter prior
    prior <- stPrior
  }
  
  #Get the relevant posteriors
  x <- posteriors[,i]
  #Plot the prior
  dens(prior, col="#00BFC4", ylim=c(0,2.1), xlim=c(-5,5), lwd=2,
       ann = F, cex.axis=0.75, xaxp  = c(-5, 5, 10))
  #Add the posterior
  dens(x, show.HPDI = 0.89, col="#F9766D", add = T, lwd=2,
       ann = F, cex.axis=0.75)
  #Add the axis labels
  mtext(side = 1, text = "Parameter Estimates (logOdds)", line = 2, cex=0.5)
  mtext(side = 2, text = "Probability Density", line = 2, cex=0.5)
  #Add the relevant title
  title(mainTitles[i], cex.main=0.75)
  #Add a line for 0
  abline(v = 0, lty=2)
  #Add a line for the median
  abline(v = median(x))
  
  #Add the posterior median text
  xMedian <- round(median(x), digits = 2)
  medianText <- paste("Median = ", as.character(xMedian), sep = "")
  text(x=5.5, y=2, labels = medianText, cex = 0.75, pos = 2)
  #Add the frequentist median text
  freqValue <- round(freqValues[i], digits = 2)
  freqText <- paste("(Frequentist = ", as.character(freqValue), ")", sep = "")
  text(x=5.7, y=1.75, labels = freqText, cex = 0.75, pos = 2)
  
  #Add the BF10 text
  BF <- round(BFs$BF[i], digits = 2)
  BFtext <- paste("BF10 = ", formatC(BF, digits = 2, format = "f"), sep = "")
  text(x=-5.5, y=2, labels = BFtext, cex = 0.75, pos = 4)
  #Add the BF01 text if necessary
  if(BF < 1) {
    BF0 <- round(1/BFs$BF[i], digits = 2)
    BF0text <- paste("BF01 = ", formatC(BF0, digits = 2, format = "f"), sep = "")
    text(x=-5.5, y=1.75, labels = BF0text, cex = 0.75, pos = 4)
  }
}
dev.off()

################################################################################
#                            Secondary logistic model                          #
################################################################################

########################################
# Creating the translucency-prediction #
########################################
# This would be alpha*beta*b and how much it is greater than c
# alpha is the level of translucency (20% or 80%)
# beta is the belief that the other participant will cooperate
# b is the benefit of cooperating = 0.5
# c is the cost of cooperating = 0.1
b <- 0.5
c <- 0.1
data$translucency_prediction <- NA
for (i in 1:nrow(data)) {
  
  beta <- data$prediction_other_cooperate[i]
  
  if(data$translucency_level[i] == "20%"){
    alpha <- 0.2
  } else {
    alpha <- 0.8
  }
  
  data$translucency_prediction[i] <- (alpha*beta*b - c)*10
}

#Mean and sd
mean(data$translucency_prediction)
sd(data$translucency_prediction)

###############
# Frequentist #
###############

#Simple model
F.2ndLM.0 <- glm(cooperation_initial ~ 1+translucency_prediction, 
                 data = data, family = "binomial")
summary(F.2ndLM.0)

#Model with prediction + direction
F.2ndLM.1 <- glm(cooperation_initial ~ 1+translucency_prediction+translucency_direction, 
                 data = data, family = "binomial")
summary(F.2ndLM.1)

#Model with interaction with direction
F.2ndLM.2 <- glm(cooperation_initial ~ 1+translucency_prediction*translucency_direction, 
                 data = data, family = "binomial")
summary(F.2ndLM.2)
exp(F.2ndLM.2$coefficients)
confint(F.2ndLM.2)

#Comparisons
logisticModelCompare(F.MainLM.intercept, F.2ndLM.2)

logisticModelCompare(F.2ndLM.0, F.2ndLM.2)

logisticModelCompare(F.2ndLM.1, F.2ndLM.2)

#Getting data split along the direction of translucency to see the effects whithin them.
dataAsym <- subset(data, data$translucency_direction == "asymmetric-defection")
F.2ndLM.asym <- glm(cooperation_initial ~ 1+translucency_prediction, 
                    data = dataAsym, family = "binomial")
summary(F.2ndLM.asym)
exp(F.2ndLM.asym$coefficients)
confint(F.2ndLM.asym)

dataSym <- subset(data, data$translucency_direction == "symmetric")
F.2ndLM.sym <- glm(cooperation_initial ~ 1+translucency_prediction, 
                   data = dataSym, family = "binomial")
summary(F.2ndLM.sym)
exp(F.2ndLM.sym$coefficients)
confint(F.2ndLM.sym)

####Checking the assumptions:####

#Getting the variables needed
assumptions <- data.frame(
  choice = data$cooperation_initial_N,
  prediction = F.2ndLM.2$fitted.values,
  translucency_prediction = data$translucency_prediction,
  translucency_direction = data$translucency_direction
)

#Add the logit
assumptions <- assumptions %>%
  mutate(logit = log(prediction/(1-prediction)))

#Plot predictor values vs logit to test linearity assumption
ggplot(assumptions, aes(logit, translucency_prediction))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  labs(x = "Logit of the Outcome", y = "Translucency Prediction") +
  theme_minimal() + 
  facet_wrap(~translucency_direction)
#No issues

ggsave(filename = "graphs/translucency_prediction_diagnosis.png",
       plot = last_plot(),
       width = 6,
       height = 3.708,
       dpi = 1200)

#Checking multicollinearity
vif(F.2ndLM.2)
1/vif(F.2ndLM.2)
#No issues

#Checking for outliers with cook's distance
cooksd = cooks.distance(F.2ndLM.2)
cookOut = which(cooksd >= 1)
#No issues

####Graph the predictions of the interaction model####
predictionPlot <- data.frame(
  choice = data$cooperation_initial_N,
  prediction = F.2ndLM.2$fitted.values,
  translucency_prediction = data$translucency_prediction,
  translucency_direction = data$translucency_direction
)

ggplot(predictionPlot, aes(x=translucency_prediction, y=prediction, group=translucency_direction, colour=translucency_direction)) + 
  geom_line() +
  geom_point(aes(x=translucency_prediction, y=choice), alpha = 0.50) +
  geom_vline(xintercept=0, linetype="dashed") + 
  scale_y_continuous(labels = scales::number_format(accuracy = .01),
                     breaks= seq(0, 1, by = .05)) +
  #coord_fixed(ratio = 2.5/1.545, ylim = c(0, 1)) + 
  labs(x = "Translucency Prediction", y = "Predicted Cooperation Levels \n (probability)", 
       color = "Direction of Translucency") + 
  theme_minimal()

ggsave(filename = "graphs/translucency_prediction_interaction.png",
       plot = last_plot(),
       width = 6,
       height = 3.708,
       dpi = 1200)

############
# Bayesian #
############

#Creating the model
B.2ndLM.2 <- stan_glm(
  cooperation_initial ~ translucency_prediction*translucency_direction,
  data = data,
  family = binomial(link = "logit"),
  prior = predictorsPrior, prior_intercept = interceptPrior, 
  chains=4, 
  seed = 17082020)

#General model info
summary(B.2ndLM.2, digits=3)

#Checking the traces of the model
mcmc_trace(B.2ndLM.2)

ggsave(filename = "graphs/bayes_prediction_mcmc.png",
       plot = last_plot(),
       width = 6,
       height = 3.708,
       dpi = 1200)

#Detailed information
describe_posterior(
  B.2ndLM.2,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

#Get the hdi
bayestestR::hdi(B.2ndLM.2, ci = .89)

#Get the Bayes factors for the model parameters
BFs <- bayesfactor_parameters(B.2ndLM.2)
BFs

#Get the posteriors from the model
posteriors <- as.data.frame(B.2ndLM.2)
dim(posteriors)

###Plot the model###
#Prepare titles for the different plots
mainTitles <- c("Intercept",
                "Translucency Prediction",
                "Translucency Direction",
                "Direction x Prediction")
#Get the frequentist values of the parameters
freqValues <- F.2ndLM.2$coefficients

dev.off()
png("graphs/translucency_prediction_overall_bayesian.png", height = 9.271, width = 15, res = 300, units = 'cm')
par(mfrow=c(2,2), mar = c(3,3,2,1))

for (i in 1:4) {
  
  if (i == 1) {
    #give the intercept prior
    prior <- nPrior
  } else {
    #give the parameter prior
    prior <- stPrior
  }
  
  #Get the relevant posteriors
  x <- posteriors[,i]
  #Plot the prior
  dens(prior, col="#00BFC4", ylim=c(0,2.1), xlim=c(-5,5), lwd=2,
       ann = F, cex.axis=0.75, xaxp  = c(-5, 5, 10))
  #Add the posterior
  dens(x, show.HPDI = 0.89, col="#F9766D", add = T, lwd=2,
       ann = F, cex.axis=0.75)
  #Add the axis labels
  mtext(side = 1, text = "Parameter Estimates (logOdds)", line = 2, cex=0.5)
  mtext(side = 2, text = "Probability Density", line = 2, cex=0.5)
  #Add the relevant title
  title(mainTitles[i], cex.main=0.75)
  #Add a line for 0
  abline(v = 0, lty=2)
  #Add a line for the median
  abline(v = median(x))
  
  #Add the posterior median text
  xMedian <- round(median(x), digits = 2)
  medianText <- paste("Median = ", as.character(xMedian), sep = "")
  text(x=5.5, y=2, labels = medianText, cex = 0.75, pos = 2)
  #Add the frequentist median text
  freqValue <- round(freqValues[i], digits = 2)
  freqText <- paste("(Frequentist = ", as.character(freqValue), ")", sep = "")
  text(x=5.7, y=1.75, labels = freqText, cex = 0.75, pos = 2)
  
  #Add the BF10 text
  BF <- round(BFs$BF[i], digits = 2)
  BFtext <- paste("BF10 = ", formatC(BF, digits = 2, format = "f"), sep = "")
  text(x=-5.5, y=2, labels = BFtext, cex = 0.75, pos = 4)
  #Add the BF01 text if necessary
  if(BF < 1) {
    BF0 <- round(1/BFs$BF[i], digits = 2)
    BF0text <- paste("BF01 = ", formatC(BF0, digits = 2, format = "f"), sep = "")
    text(x=-5.5, y=1.75, labels = BF0text, cex = 0.75, pos = 4)
  }
}
dev.off()

####Test the role of translucency_predictor within the asymmetric-defection####

#Creating the model
B.2ndLM.asym <- stan_glm(
  cooperation_initial ~ translucency_prediction,
  data = dataAsym,
  family = binomial(link = "logit"),
  prior = predictorsPrior, prior_intercept = interceptPrior, 
  chains=4, 
  seed = 17082020)

#General model info
summary(B.2ndLM.asym, digits = 3)

#Checking the traces of the model
mcmc_trace(B.2ndLM.asym)

ggsave(filename = "graphs/bayes_prediction_asym_mcmc.png",
       plot = last_plot(),
       width = 6,
       height = 3.708,
       dpi = 1200)

#Detailed information
describe_posterior(
  B.2ndLM.asym,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

#Get the hdi
bayestestR::hdi(B.2ndLM.asym, ci = .89)

#Get the Bayes factors for the model parameters
BFs <- bayesfactor_parameters(B.2ndLM.asym)
BFs

#Get the posteriors from the model
posteriors <- as.data.frame(B.2ndLM.asym)
dim(posteriors)

#Prepare titles for the different plots
mainTitles <- c("Intercept",
                "Translucency Prediction")
#Get the frequentist values of the parameters
freqValues <- F.2ndLM.asym$coefficients

dev.off()
png("graphs/translucency_prediction_asym_bayesian.png", height = 12.361, width = 20, res = 300, units = 'cm')
par(mfrow=c(1,2), mar = c(3,3,2,1))

for (i in 1:2) {
  
  if (i == 1) {
    #give the intercept prior
    prior <- nPrior
  } else {
    #give the parameter prior
    prior <- stPrior
  }
  
  #Get the relevant posteriors
  x <- posteriors[,i]
  #Plot the prior
  dens(prior, col="#00BFC4", ylim=c(0,2.1), xlim=c(-5,5), lwd=2,
       ann = F, cex.axis=0.80, xaxp  = c(-5, 5, 10))
  #Add the posterior
  dens(x, show.HPDI = 0.89, col="#F9766D", add = T, lwd=2,
       ann = F, cex.axis=0.80)
  #Add the axis labels
  mtext(side = 1, text = "Parameter Estimates (logOdds)", line = 2, cex=0.75)
  mtext(side = 2, text = "Probability Density", line = 2, cex=0.75)
  #Add the relevant title
  title(mainTitles[i], cex.main=0.80)
  #Add a line for 0
  abline(v = 0, lty=2)
  #Add a line for the median
  abline(v = median(x))
  
  #Add the posterior median text
  xMedian <- round(median(x), digits = 2)
  medianText <- paste("Median = ", formatC(xMedian, digits = 2, format = "f"), sep = "")
  text(x=5.5, y=2.1, labels = medianText, cex = 0.80, pos = 2)
  #Add the frequentist median text
  freqValue <- round(freqValues[i], digits = 2)
  freqText <- paste("(Frequentist = ", formatC(freqValue, digits = 2, format = "f"), ")", sep = "")
  text(x=5.7, y=2, labels = freqText, cex = 0.80, pos = 2)
  
  #Add the BF10 text
  BF <- round(BFs$BF[i], digits = 2)
  BFtext <- paste("BF10 = ", formatC(BF, digits = 2, format = "f"), sep = "")
  text(x=-5.5, y=2.1, labels = BFtext, cex = 0.80, pos = 4)
  #Add the BF01 text if necessary
  if(BF < 1) {
    BF0 <- round(1/BFs$BF[i], digits = 2)
    BF0text <- paste("BF01 = ", formatC(BF0, digits = 2, format = "f"), sep = "")
    text(x=-5.5, y=2, labels = BF0text, cex = 0.80, pos = 4)
  }
}
dev.off()

####Test the role of translucency_predictor within the symmetric directions####

#Creating the model
B.2ndLM.sym <- stan_glm(
  cooperation_initial ~ translucency_prediction,
  data = dataSym,
  family = binomial(link = "logit"),
  prior = predictorsPrior, prior_intercept = interceptPrior, 
  chains=4, 
  seed = 17082020)

#General model info
summary(B.2ndLM.sym, digits = 3)

#Checking the traces of the model
mcmc_trace(B.2ndLM.sym)

ggsave(filename = "graphs/bayes_prediction_sym_mcmc.png",
       plot = last_plot(),
       width = 6,
       height = 3.708,
       dpi = 1200)

#Detailed information
describe_posterior(
  B.2ndLM.sym,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

#Get the hdi
bayestestR::hdi(B.2ndLM.sym, ci = .89)

#Get the Bayes factors for the model parameters
BFs <- bayesfactor_parameters(B.2ndLM.sym)
BFs

#Get the posteriors from the model
posteriors <- as.data.frame(B.2ndLM.sym)
dim(posteriors)

#Prepare titles for the different plots
mainTitles <- c("Intercept",
                "Translucency Prediction")
#Get the frequentist values of the parameters
freqValues <- F.2ndLM.sym$coefficients

dev.off()
png("graphs/translucency_prediction_sym_bayesian.png", height = 12.361, width = 20, res = 300, units = 'cm')
par(mfrow=c(1,2), mar = c(3,3,2,1))

for (i in 1:2) {
  
  if (i == 1) {
    #give the intercept prior
    prior <- nPrior
  } else {
    #give the parameter prior
    prior <- stPrior
  }
  
  #Get the relevant posteriors
  x <- posteriors[,i]
  #Plot the prior
  dens(prior, col="#00BFC4", ylim=c(0,2.1), xlim=c(-5,5), lwd=2,
       ann = F, cex.axis=0.80, xaxp  = c(-5, 5, 10))
  #Add the posterior
  dens(x, show.HPDI = 0.89, col="#F9766D", add = T, lwd=2,
       ann = F, cex.axis=0.80)
  #Add the axis labels
  mtext(side = 1, text = "Parameter Estimates (logOdds)", line = 2, cex=0.75)
  mtext(side = 2, text = "Probability Density", line = 2, cex=0.75)
  #Add the relevant title
  title(mainTitles[i], cex.main=0.80)
  #Add a line for 0
  abline(v = 0, lty=2)
  #Add a line for the median
  abline(v = median(x))
  
  #Add the posterior median text
  xMedian <- round(median(x), digits = 2)
  medianText <- paste("Median = ", formatC(xMedian, digits = 2, format = "f"), sep = "")
  text(x=5.5, y=2.1, labels = medianText, cex = 0.80, pos = 2)
  #Add the frequentist median text
  freqValue <- round(freqValues[i], digits = 2)
  freqText <- paste("(Frequentist = ", formatC(freqValue, digits = 2, format = "f"), ")", sep = "")
  text(x=5.7, y=2, labels = freqText, cex = 0.80, pos = 2)
  
  #Add the BF10 text
  BF <- round(BFs$BF[i], digits = 2)
  BFtext <- paste("BF10 = ", formatC(BF, digits = 2, format = "f"), sep = "")
  text(x=-5.5, y=2.1, labels = BFtext, cex = 0.80, pos = 4)
  #Add the BF01 text if necessary
  if(BF < 1) {
    BF0 <- round(1/BFs$BF[i], digits = 2)
    BF0text <- paste("BF01 = ", formatC(BF0, digits = 2, format = "f"), sep = "")
    text(x=-5.5, y=2, labels = BF0text, cex = 0.80, pos = 4)
  }
}
dev.off()


################################################################################
#                             Beliefs about others                            #
################################################################################

getSSE <- function(model) {
  SSE <- sum(residuals(model)^2)
  return(SSE)
}

getSSR <- function(model1, model2) {
  SSE1 <- sum(residuals(model1)^2)
  SSE2 <- sum(residuals(model2)^2)
  
  if(SSE1 >= SSE2){
    SSE_C <- SSE1
    SSE_A <- SSE2
  } else {
    SSE_C <- SSE2
    SSE_A <- SSE1
  }
  
  SSR <- SSE_C - SSE_A
  
  return(SSR)
}

getPRE <- function(model1, model2) {
  SSE1 <- sum(residuals(model1)^2)
  SSE2 <- sum(residuals(model2)^2)
  
  if(SSE1 >= SSE2){
    SSE_C <- SSE1
    SSE_A <- SSE2
  } else {
    SSE_C <- SSE2
    SSE_A <- SSE1
  }
  
  SSR <- SSE_C - SSE_A
  PRE <- SSR/SSE_C
  
  return(PRE)
}

f_test <- function(SSR, SSE, df1, df2){
  f <- (SSR/df1)/(SSE/df2)
  p <- 1-pf(f, df1 = df1, df2 = df2)
  PRE <- SSR/SSE
  
  print(paste("F(", df1, ",", df2, ") = ", round(f, digits = 2), sep = "")) 
  print(paste("p = ", round(p, digits = 3), sep = "")) 
  print(paste("PRE = ", round(PRE, digits = 3), sep = ""))
}

#######################################################
# Cooperation predictions affected by the conditions? #
#######################################################

#General mean and sd
mean(data$prediction_other_cooperate)
sd(data$prediction_other_cooperate)

#Making and comparing the models
beliefMod0 <- lm(prediction_other_cooperate ~ 1, data=data)
beliefMod1 <- lm(prediction_other_cooperate ~ translucency_level, data=data)
beliefMod2 <- lm(prediction_other_cooperate ~ translucency_level+translucency_direction, data=data)
beliefMod3 <- lm(prediction_other_cooperate ~ translucency_level*translucency_direction, data=data)

#Unequal groups we need Type 1 ANOVA
#Manual comparisons because 
SSR_level <- getSSR(beliefMod0, beliefMod1)
SSR_direction <- getSSR(beliefMod1, beliefMod2)
SSR_interaction <- getSSR(beliefMod2, beliefMod3)
SSR_overall <- SSR_level + SSR_direction + SSR_interaction
SSE <- getSSE(beliefMod3)
df2 <- nrow(data) - 4

#Omnibus test
f_test(SSR_overall, SSE, 3, df2)
#Interaction
f_test(SSR_interaction, SSE, 1, df2)
#Level
f_test(SSR_level, SSE, 1, df2)
#Direction
f_test(SSR_direction, SSE, 1, df2)

#Using aov
summary(aov(prediction_other_cooperate ~ translucency_level*translucency_direction, data=data))

#Getting the CI
round(confint(aov(prediction_other_cooperate ~ translucency_level*translucency_direction, data=data)), digits = 2)

#Getting the aggregate means and SDs
aggregate(prediction_other_cooperate ~ translucency_level, FUN = mean, data = data)
aggregate(prediction_other_cooperate ~ translucency_level, FUN = sd, data = data)

aggregate(prediction_other_cooperate ~ translucency_direction, FUN = mean, data = data)
aggregate(prediction_other_cooperate ~ translucency_direction, FUN = sd, data = data)

#Testing assumptions of the main model
leveneTest(aov(prediction_other_cooperate ~ translucency_direction*translucency_level, data=data), center=mean)

mod.residuals <- residuals(beliefMod3)
mod.predictors <- predict(beliefMod3)

dev.off()
png("graphs/assump_belief_general.png", height = 12.361, width = 20, res = 300, units = 'cm')
par(mfrow=c(1,2), mar = c(5,4,2,1))
plot(mod.predictors, mod.residuals, 
     ylab = "Residuals",
     xlab = "Predictions",
     main = "(a)")
abline(h=mean(mod.residuals))

qqnorm(mod.residuals, main = "(b)")
qqline(mod.residuals)
dev.off()

cooksd = cooks.distance(beliefMod3)
cookOut = which(cooksd >= 1)

#thin tails
#https://seankross.com/2016/02/29/A-Q-Q-Plot-Dissection-Kit.html

##############################################################
# Cooperation if see predictions affected by the conditions? #
##############################################################

#General mean and sd
mean(data$prediction_other_cooperate_if_saw)
sd(data$prediction_other_cooperate_if_saw)

#Making and comparing the models
beliefMod0 <- lm(prediction_other_cooperate_if_saw ~ 1, data=data)
beliefMod1 <- lm(prediction_other_cooperate_if_saw ~ translucency_level, data=data)
beliefMod2 <- lm(prediction_other_cooperate_if_saw ~ translucency_level+translucency_direction, data=data)
beliefMod3 <- lm(prediction_other_cooperate_if_saw ~ translucency_level*translucency_direction, data=data)

#Unequal groups we need Type 1 ANOVA
#Manual comparisons because 
SSR_level <- getSSR(beliefMod0, beliefMod1)
SSR_direction <- getSSR(beliefMod1, beliefMod2)
SSR_interaction <- getSSR(beliefMod2, beliefMod3)
SSR_overall <- SSR_level + SSR_direction + SSR_interaction
SSE <- getSSE(beliefMod3)
df2 <- nrow(data) - 4

#Omnibus test
f_test(SSR_overall, SSE, 3, df2)
#Interaction
f_test(SSR_interaction, SSE, 1, df2)
#Level
f_test(SSR_level, SSE, 1, df2)
#Direction
f_test(SSR_direction, SSE, 1, df2)

#Using aov
summary(aov(prediction_other_cooperate_if_saw ~ translucency_level*translucency_direction, data=data))

#Getting the CI
round(confint(aov(prediction_other_cooperate_if_saw ~ translucency_level*translucency_direction, data=data)), digits = 2)

#Getting the aggregate means and SDs
aggregate(prediction_other_cooperate_if_saw ~ translucency_level, FUN = mean, data = data)
aggregate(prediction_other_cooperate_if_saw ~ translucency_level, FUN = sd, data = data)

aggregate(prediction_other_cooperate_if_saw ~ translucency_direction, FUN = mean, data = data)
aggregate(prediction_other_cooperate_if_saw ~ translucency_direction, FUN = sd, data = data)

#Testing assumptions of the main model
leveneTest(aov(prediction_other_cooperate_if_saw ~ translucency_direction*translucency_level, data=data), center=mean)

mod.residuals <- residuals(beliefMod3)
mod.predictors <- predict(beliefMod3)

dev.off()
png("graphs/assump_belief_see.png", height = 12.361, width = 20, res = 300, units = 'cm')
par(mfrow=c(1,2), mar = c(5,4,2,1))
plot(mod.predictors, mod.residuals, 
     ylab = "Residuals",
     xlab = "Predictions",
     main = "(a)")
abline(h=mean(mod.residuals))

qqnorm(mod.residuals, main = "(b)")
qqline(mod.residuals)
dev.off()

cooksd = cooks.distance(beliefMod3)
cookOut = which(cooksd >= 1)

################################################################################
#                                   Vision                                     #
################################################################################

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
translucency_direction <- rep(NA, nrow(data)*2)
translucency_level <- rep(NA, nrow(data)*2)
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
    choice[index] <- data$cooperation_initial[i]
    #Get translucency level
    translucency_level[index] <- data$translucency_level[i]
    #Get translucency direction
    translucency_direction[index] <- data$translucency_direction[i]
    
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
  movement_prediction,
  translucency_level,
  translucency_direction))

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

visionData$translucency_level <- factor(visionData$translucency_level, 
                                        levels = c(1, 2), 
                                        labels = c("20%", "80%"))

visionData$translucency_direction <- factor(visionData$translucency_direction, 
                                            levels = c(1, 2), 
                                            labels = c("asymmetric-defection", "symmetric"))

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

#####################################
# Fixed effects and interpretations #
#####################################

reml.3 <- lmer(predicted_Cooperation~1+vision_condition*choice + (1|id), 
               data = visionData, REML = T)
summary(reml.3)
confint(reml.3)

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
png("graphs/translucency_belief_assumptions.png", height = 12.361, width = 20, res = 300, units = 'cm')
par(mfrow=c(1,2), mar = c(5,4,2,1))
plot(mod.predictors, mod.residuals, 
     ylab = "Residuals",
     xlab = "Predictions",
     main = "(a)")
abline(h=mean(mod.residuals))

qqnorm(mod.residuals, main = "(b)")
qqline(mod.residuals)
dev.off()

cooksd = cooks.distance(reml.1)
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
               fun = mean, geom="line", size = 1) +
  stat_summary(aes(x = vision_condition, y=predicted_Cooperation, group=choice),
               fun = mean, geom="point", size = 2) +
  facet_wrap(~choice) + 
  scale_y_continuous(labels = scales::number_format(accuracy = .01),
                     breaks= seq(0, 1, by = .05)) +
  coord_fixed(ratio = 3/1, ylim = c(0, 1)) + 
  geom_hline(yintercept=0.5, linetype="dashed") +
  labs(x = "Conditions for Reporting Belief", y = "Belief Other Will Cooperate \n (probability)", 
       color = "Change Between Conditions \n for Reporting Belief") + 
  theme_minimal() + 
  theme(legend.position="bottom")

ggsave(filename = "graphs/translucency_beliefs_general.png",
       plot = last_plot(),
       width = 12,
       height = 7.416,
       dpi = 1200)

###########################
# Clusters of differences #
###########################

data$difference <- data$prediction_other_cooperate_if_saw - data$prediction_other_cooperate

ggplot(data = data, aes(y=difference, x=cooperation_initial)) +
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

ggsave(filename = "graphs/translucency_beliefs_differences.png",
       plot = last_plot(),
       width = 6,
       height = 5,
       dpi = 1200)

################################################################################
#                          Comparing rate with control                         #
################################################################################
#Preparing data to compare with control group

mixedData <- data.frame(
  ID = data$ID + 31,
  cooperation = data$cooperation_initial_N,
  experiment = data$experiment
)

write.csv(mixedData, "compareCnE/experimental.csv", row.names = F)