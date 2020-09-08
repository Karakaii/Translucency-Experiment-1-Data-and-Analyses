## First Experimental Investigation of Translucency Theory: Data and Analyses

### Power Analysis

To see the results and code of our power analysis go to [/Power Analysis](https://github.com/Karakaii/Translucency-Experiment-1-Data-and-Analyses/tree/master/Power%20Analysis)

### Preregistration

For our preregistered aims, methods, analyses, and exclusion criteria with AsPredicted see [AsPredicted (for peer-review only) #45400.pdf](https://github.com/Karakaii/Translucency-Experiment-1-Data-and-Analyses/blob/master/AsPredicted%20(for%20peer-review%20only)%20%2345400.pdf) 

### Choosing the payoffs

To choose the payoffs for our PD we searched the literature for other studies conducted with a one-shot, normal form, double-choice PD which did not have strong manipulations of certain factors (e.g., did not elicit participants’ beliefs about the other participant’s cooperation before making their choice). For each study selected we recorded their payoffs, benefit to cost of cooperation ratio, and cooperation rate. We also calculated the translucency level α above which it is translucently rational to cooperate according to the studies’ b and c, where β was the actual rate of cooperation (i.e., as if the participant knew the rate of cooperation, although this is a flawed measure of the theoretical value of β because, according to translucency, the cooperation rate will have been affected by translucency beliefs), and where α≥c/βb. 

You can see a table summary of these findings and calculations [here](https://github.com/Karakaii/Translucency-Experiment-1-Data-and-Analyses/blob/master/table%20research%20for%20payoffs.xlsx)
### Data and Analyses

Go to [/Data & Analyses](https://github.com/Karakaii/Translucency-Experiment-1-Data-and-Analyses/tree/master/Data%20%26%20Analyses)

Here, the prefix *control* means the control group, and the prefix *translucency* means the experimental group.

The 'raw' anonymised data from our online experiment (mostly under the jsPsych format) are:
- control_anonym_data.csv
- translucency_anonym_data.csv

The scripts *control_make_data.R* and *translucency_make_data.R* take the anonymised data and format them so that they are easier to analyses (and only include variables of interest) into:
- control_analysis_data_all.csv
- translucency_analysis_data_all.csv

The data analysis is then conducted with the following scripts:
- control_analysis_all.R
- control_analysis_data_all.jasp
- translucency_analysis_all.R

In */graphs* you will see the graphs produced by the scripts.
In */compare control and experimental* you will see the selected data and the analysis script for comparing cooperation rates across the control and experimental groups.

In */understood_payoffs* you will find an equivalent of all the elements in this main folder, but only including the participants who answered correctly to both of our questions about the payoffs.

### Code for our Online Prisoner's Dilemma

Please see this [repository](https://github.com/Karakaii/Translucency-Experiment-1-Code-For-Multiplayer-Prisoner-s-Dilemma/) for our online Prisoner's Dilemma code.
- For the [control group](https://github.com/Karakaii/Translucency-Experiment-1-Code-For-Multiplayer-Prisoner-s-Dilemma/releases/tag/v1.0-Control)
- For the [experimental group](https://github.com/Karakaii/Translucency-Experiment-1-Code-For-Multiplayer-Prisoner-s-Dilemma/releases/tag/v2.0-Translucency)