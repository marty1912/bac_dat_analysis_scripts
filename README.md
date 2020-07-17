## DATA ANALYSIS GROUP 1

in this folder you can find all the scripts used to analize the data for group 1

# how it works:

if you are on mac or on linux you can run the shellscript "merge_and_zip.sh" by exexuting it in a terminal.
type:

```
./merge_and_zip.sh
```

the shell script will execute a few R scripts that create the data files we need for our analysis.
afterwards all data files are added to an zip archive.

if you are using windows you can install the ubuntu subsystem for windows or similar from the microsoft store after you enable developer mode.

## the following scripts are used: 
 - merge.r: merges all the files from the data folder into one big .csv
 - calc_accuracy.r: calculates the accuracy for each participant.
 - clean_data.r: cleans data (removes based on demographics,...)
 - calc_rev_dist.r: adds reverse distance effect to the data and finishes cleaning (only complete cases are used)

## The final (useful) datafiles are:
 - data_accuracy.csv: has all the data on accuracy. Also from participants who are excluded from the final analysis.
 - data_accuracy_p_participant_dual.csv: has data on accuracy for the dual tasks (visual and phonological). 
 - data_cleaned_complete.csv: has all data of complete cases in long format (for R).
 - data_rev_dist_effect_1rpP.csv: has data on reverse distance effect for rt, acc and ies in wide format (for jamovi).

## Analysis

You can find the final analysis html output in the [releases](https://github.com/marty1912/bac_dat_analysis_scripts/releases) section.

Also if you run ./merge_and_zip.sh you can do the analysis using the R markdown script an_final.Rmd. 
To create a html analysis file open it with R studio and press the knit button.


# Requirements
for the merge and zip part tidyverse is required.
for the analysis you need R 4.0 and a few libraries (just check in the beginning of the file)
