## Data and Analyses

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
