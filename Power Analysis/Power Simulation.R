###SetUp###
rm(list = ls(all = TRUE))

################################################################################
#                   First try with just one sample size                        #
################################################################################

#Sample size
sample <- 1000

#Getting the translucency conditions (-1 is small, 1 is large)
transluency <- sample(c(-1,1), size = sample, replace = TRUE)

#Building the logistic regression model
#The betas represent changes in log odds per unit change in the predictors
# I want low translucency to be 40% cooperation. And high translucency to be 50% cooperation.
low_odds <- .4/(1-.4)
low_log_odds <- log(low_odds) 

high_odds <- .5/(1-.5)
high_log_odds <- log(high_odds)

#when small alpha, to get 40% cooperation, the mean log odds set the b0 to
b0 <- (low_log_odds -  high_log_odds)/2

#as alpha increases, cooperation increases, the log 
b1 <- (-1*low_log_odds +  1*high_log_odds)/2 
#make as odds ratio, this means it is x times more likely to cooperate when large than small alpha
exp(b1) 

#The model
xb <- b0 + b1*transluency

#Probability of cooperating if small alpha
p_defectLargeA <- 1/(1 + exp(-(b0 + b1*-1)))
p_defectLargeA

#Probability of cooperating if large alpha
p_defectLargeA <- 1/(1 + exp(-(b0 + b1*1)))
p_defectLargeA

#Based on the logistic regression model, calculate the probability of cooperating
p <- 1/(1 + exp(-xb))
summary(p) #check that it lies between 0 and 1

#Simulate observations of defection (0) or cooperation (1)
coopRate <- rbinom(n = sample, size = 1, prob = p)
#Proportion of cooperation
prop.table(table(coopRate))

#Run logistic regression on data to check that the "true" betas correspond to those we selected
mod <- glm(coopRate ~ transluency, family = "binomial")
summary(mod)
s.out <-summary(mod)
s.out$coefficients["transluency", "Pr(>|z|)"] < 0.05
# Works if values are within 2*SE of estimated values

################################################################################
#                           Multiple Simulations                               #
################################################################################

#Function to run the simulation with a certain sample
simFit <- function(sample){
  
  #Getting the translucency conditions (-1 is small, 1 is large)
  transluency <- sample(c(-1,1), size = sample, replace = TRUE)
  
  #Building the logistic regression model
  #The betas represent changes in log odds per unit change in the predictors
  # I want low translucency to be 40% cooperation. And high translucency to be 50% cooperation.
  low_odds <- .4/(1-.4)
  low_log_odds <- log(low_odds) 
  
  high_odds <- .5/(1-.5)
  high_log_odds <- log(high_odds)
  
  #when small alpha, to get 40% cooperation, the mean log odds set the b0 to
  b0 <- (low_log_odds -  high_log_odds)/2
  
  #as alpha increases, cooperation increases, the log 
  b1 <- (-1*low_log_odds +  1*high_log_odds)/2 
  #make as odds ratio, this means it is x times more likely to cooperate when large than small alpha
  exp(b1) 
  
  #The model
  xb <- b0 + b1*transluency
  
  #Probability of cooperating if small alpha
  p_defectLargeA <- 1/(1 + exp(-(b0 + b1*-1)))
  p_defectLargeA
  
  #Probability of cooperating if large alpha
  p_defectLargeA <- 1/(1 + exp(-(b0 + b1*1)))
  p_defectLargeA
  
  #Based on the logistic regression model, calculate the probability of cooperating
  p <- 1/(1 + exp(-xb))
  summary(p) #check that it lies between 0 and 1
  
  #Simulate observations of defection (0) or cooperation (1)
  coopRate <- rbinom(n = sample, size = 1, prob = p)
  #Proportion of cooperation
  prop.table(table(coopRate))
  
  #Run logistic regression on data to check that the "true" betas correspond to those we selected
  mod <- glm(coopRate ~ transluency, family = "binomial")
  
  #save the output
  s.out <- summary(mod)
  
  #Check significance
  s.out$coefficients["transluency", "Pr(>|z|)"] < 0.05
}

#function to run simulations and outputs the power
powerEst <- function(N, n){
  r.out <- replicate(n = N, simFit(sample = n))
  mean(r.out)
}

#Running 500 simulations with sample size of 100
powerEst(N = 500, n = 100)
# > powerEst(N = 500, n = 100)
# [1] 0.174
#This is very low power

#Let's run powerEst on a range of sample sizes.
#Range of sample sizes from 100 to 1000 by increments of 20
ss <- seq(100, 1000, 20)
#Running the power function across these sample sizes
p.out <- sapply(ss, function(x)powerEst(N = 500, n = x))
#Outputting the sample sizes where the power is above 0.8
ss[p.out > 0.8]
#Visualize the relation between power and sample size
plot(ss, p.out, type = "l", 
     xlab = "sample size", ylab = "power")
abline(h = 0.8, lty = 2)