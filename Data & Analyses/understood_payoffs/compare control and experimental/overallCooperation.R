
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

#Getting the data files
dataControl <- read.csv("control.csv")
dataExperimental <- read.csv("experimental.csv")

#Merging them
data <- rbind(dataControl, dataExperimental)

#Setting factors
data$experiment <- factor(data$experiment, 
                                levels = c(1, 2), 
                                labels = c("control", "experimental"))

data$cooperation <- factor(data$cooperation, 
                           levels = c(0, 1), 
                           labels = c("defection", "cooperation"))

#Comparing cooperation in a logistic model
logisticM <- glm(cooperation ~ experiment, 
                 data = data, family = "binomial")
summary(logisticM)
exp(logisticM$coefficients)
confint(logisticM)
