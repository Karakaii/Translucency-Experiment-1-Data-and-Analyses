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
#install.packages("rjson")
library(rjson)
#install.packages("stringr")
library(stringr)

####Getting the data####
data <- read.csv("control_anonym_data.csv")

###Function to make variable from JSON string ###
#NOTE: only works if the JSON string contains one element/one question
JSON.parse <- function(json){
  #Pre-allocate the vector
  output <- rep(NA, length(json))
  for (i in 1:length(json)) {
    #Parse the JSON string into a list for each row
    output[i] <- fromJSON(json_str = json[i])
  }
  #Unlist the string so that it is a vector with chr variables
  output <- unlist(output)
  return(output)
}

################################################################################
#                          Preparing Data for Analysis                         #
################################################################################

#Start the analysis data
analysis_data <- select(data, ID, timeout, settings.condition, myChoice,
                        otherID, otherChoice)

#Is the other participant in the data set
analysis_data$isOtherIn <- !is.na(analysis_data$otherID)

#Adding information that this is the control experiment
analysis_data$experiment <- 1

####################
# Counterbalancing #
####################

#Create a variable that sets whether participant is in counter balancing
#condition A or B.
analysis_data$counterbalancing <- if_else(
  str_detect(analysis_data$settings.condition, "A"),
  0,
  1)

###############
# Cooperation #
###############

#Calculate whether participants cooperated or not
#Set cooperation to 1 and defection to 0
#In condition A, choice A is cooperating
#In condition B, choice B is cooperating

###Initial cooperation###
analysis_data$cooperation_N <- if_else((str_detect(analysis_data$settings.condition, "A") &
                                                  analysis_data$myChoice == "A") |
                                                 (str_detect(analysis_data$settings.condition, "B") &
                                                    analysis_data$myChoice == "B"), 
                                               1, 0)


#Get the cooperation of the other player
analysis_data$cooperation_other <- if_else((str_detect(analysis_data$settings.condition, "A") &
                                                    analysis_data$otherChoice == "A") |
                                                   (str_detect(analysis_data$settings.condition, "B") &
                                                      analysis_data$otherChoice == "B"), 
                                                 1, 0)

#######################################
# Predictions about other participant #
#######################################

#% how much they think the other player chose B
analysis_data$prediction_other_cooperate <- as.numeric(data$jsPsych.response)/100
#becomes % how much they think the other player cooperated
analysis_data$prediction_other_cooperate <- if_else(
  str_detect(analysis_data$settings.condition, "A"), 
  -1*(analysis_data$prediction_other_cooperate-1),
  analysis_data$prediction_other_cooperate)

#Explanation for their prediction of other player's cooperation
analysis_data$prediction_other_cooperate_explanation <- JSON.parse(data$jsPsych.responses.1)

#% how much they think the other player thinks they chose B
analysis_data$reverse_prediction_themselves_cooperate <- as.numeric(data$jsPsych.response.1)/100
#becomes % how much they think the other player thinks they cooperated
analysis_data$reverse_prediction_themselves_cooperate <- if_else(
  str_detect(analysis_data$settings.condition, "A"), 
  -1*(analysis_data$reverse_prediction_themselves_cooperate-1),
  analysis_data$reverse_prediction_themselves_cooperate)

#Explanation for their prediction of other player belief that they cooperated
analysis_data$reverse_prediction_themselves_cooperate_explanation <- JSON.parse(data$jsPsych.responses.2)

#% how much they think the other player would chose B if they could see their choice
analysis_data$prediction_other_cooperate_if_saw <- as.numeric(data$jsPsych.response.2)/100
#becomes % how much they think the other player would cooperate if they could see their choice
analysis_data$prediction_other_cooperate_if_saw <- if_else(
  str_detect(analysis_data$settings.condition, "A"), 
  -1*(analysis_data$prediction_other_cooperate_if_saw-1),
  analysis_data$prediction_other_cooperate_if_saw)

#Explanation for their prediction of other player would cooperate if they could see their choice
analysis_data$prediction_other_cooperate_if_saw_explanation <- JSON.parse(data$jsPsych.responses.3)

#According to their comment, it is clear that participant 
#31 made a mistake and inverted A and B for the question
#about what they think the other participant would do if they could see their
#choice:

#"I may have incorrectly typed 'A' instead of B on one of my responses: I meant 
#that someone will go with the option that gives them the most money, so if I 
#chose B and they knew it would be B, they would choose B for the maximum 
#payout, not A as I think I said."

#It would be strange that with this rational about people's behaviour in the 
#game, that this participant truly meant that they think there is 72% chance 
#that the other player would cooperate if they saw them defect.

# Hence, I edited their response to represent this, with 0.28 instead of 0.72
analysis_data[analysis_data$ID == 31,]$prediction_other_cooperate_if_saw <- 0.28

#########################
# Payoff Comprehension #
#########################

#The payoff comprehension questions are two questions, this will produces lines
#with two columns
for (i in 1:length(data$jsPsych.responses.4)) {
  #Extract the json string
  payoff_line <- fromJSON(json_str = data$jsPsych.responses.4[i])
  #Make into a dataframe (reverse rows and columns)
  payoff_line <- as.data.frame(t(payoff_line))
  
  #If this is the first row, start the payoff_comprehension dataframe
  if (i == 1) {
    payoff_comprehension <- payoff_line
    #If not, rbind to the payoff_comprehension dataframe
  } else {
    payoff_comprehension <- rbind(payoff_comprehension, payoff_line)
  }
}

#Test whether participants accurately answered the first question.
#If condition A: the outcome that leads to the best reward for oneself is
#"You choose B and they choose A"
#If condition B: the outcome that leads to the best reward for oneself is
#"You choose A and they choose B"
payoff_comprehension$payoffComprehension1_accuracy <- if_else(
  (str_detect(analysis_data$settings.condition, "A") & 
     payoff_comprehension$payoffComprehension1 == 
     "You choose B and they choose A") |
    (str_detect(analysis_data$settings.condition, "B") & 
       payoff_comprehension$payoffComprehension1 == 
       "You choose A and they choose B"), TRUE, FALSE)

#Test whether participants accurately answered the second question.
#If condition A: the outcome that leads to the best sum of rewards for the group
#"You choose A and they choose A"
#If condition B: he outcome that leads to the best sum of rewards for the group
#"You choose B and they choose B"
payoff_comprehension$payoffComprehension2_accuracy <- if_else(
  (str_detect(analysis_data$settings.condition, "A") & 
     payoff_comprehension$payoffComprehension2 == 
     "You choose A and they choose A") |
    (str_detect(analysis_data$settings.condition, "B") & 
       payoff_comprehension$payoffComprehension2 == 
       "You choose B and they choose B"), TRUE, FALSE)

#Reorder the payoff_comprehension dataframe
payoff_comprehension <- select(payoff_comprehension, 
                               payoffComprehension1, 
                               payoffComprehension1_accuracy, 
                               payoffComprehension2, 
                               payoffComprehension2_accuracy)

#If participants got the right answer for both questions, this means they 
#understood the payoffs. Set this new variable as TRUE.
payoff_comprehension$understood_payoffs <- if_else(
  payoff_comprehension$payoffComprehension1_accuracy & 
    payoff_comprehension$payoffComprehension2_accuracy, 
  TRUE, FALSE)

#Add this to the analysis_data
analysis_data$understood_payoffs <- payoff_comprehension$understood_payoffs

##################
# Post questions #
##################

#% How much participants believe the other player knew their choice
analysis_data$did_they_know <- as.numeric(data$jsPsych.response.3)/100

#Participant's explanation as to why the other player made this choice
analysis_data$why_other_choice <- JSON.parse(data$jsPsych.responses.5)

#% that they would choose B if they played again with the same person
analysis_data$play_coop_again_same <- as.numeric(data$jsPsych.response.4)/100
#becomes % that they would cooperate if they played again with the same person
analysis_data$play_coop_again_same <- if_else(
  str_detect(analysis_data$settings.condition, "A"), 
  -1*(analysis_data$play_coop_again_same-1),
  analysis_data$play_coop_again_same)

#% that they would choose B if they played again with a different person
analysis_data$play_coop_again_diff <- as.numeric(data$jsPsych.response.5)/100
#becomes % that they would cooperate if they played again with a different person
analysis_data$play_coop_again_diff <- if_else(
  str_detect(analysis_data$settings.condition, "A"), 
  -1*(analysis_data$play_coop_again_diff-1),
  analysis_data$play_coop_again_diff)

########################
# Experience with econ #
########################

#Likert of their experience with econ games like these
analysis_data$experience_econ_num <- as.numeric(data$jsPsych.likertResponseNumber)
analysis_data$experience_econ_str <- data$jsPsych.likertResponseText

#What they think the name of the game is
analysis_data$name_game <- JSON.parse(data$jsPsych.responses.7)

#Did they get the name of the game right?
analysis_data$know_name_game <- if_else(
  analysis_data$name_game == "Prisoner's Dilemma",
  1, 0)

################
# Demographics #
################

#Gender
analysis_data$gender_reported <- JSON.parse(data$jsPsych.responses.8)
analysis_data$gender_prolific <- data$Sex
analysis_data$gender_match <- if_else(
  analysis_data$gender_reported == analysis_data$gender_prolific,
  TRUE, FALSE)

#Age
analysis_data$age_reported <- as.numeric(data$jsPsych.response.6)
analysis_data$age_prolific <- data$age
analysis_data$age_match <- if_else(
  analysis_data$age_reported == analysis_data$age_prolific |
    analysis_data$age_reported == analysis_data$age_prolific-1 |
    analysis_data$age_reported == analysis_data$age_prolific+1,
  TRUE, FALSE)

####################
# Time information #
####################

#Time to make their choice during the choice bit
analysis_data$time_choice <- data$jsPsych.rt.2/60000 #in minutes

#Total time
analysis_data$time_total <- data$jsPsych.time_elapsed.21/60000 #in minutes

#####################
# Feedback comments #
#####################

#Get the comments from the JSON string
analysis_data$feedback_comments <- JSON.parse(data$jsPsych.responses.10) 

################################################################################
#                            Writing the data to CSVs                          #
################################################################################

#Writing the full data to a csv
write.csv(analysis_data, file="control_analysis_data_all.csv", row.names = F)

#Take out participants who did not understand payoffs
analysis_data <- subset(analysis_data, understood_payoffs)
#Writing the payoff data to a csv
write.csv(analysis_data, file="payoffs/control_analysis_data_payoffs.csv", row.names = F)