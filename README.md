This repository contains the python scripts and supporting files used to run experiments and process data for the manuscript "Ethnic Disparity in Diagnosing Asymptomatic Bacterial Vaginosis using Machine Learning" The following is a breakdown of the files and how they were used.

In the src folder is the following files:

src/experiments.py

Overall experiment comparing all models.
Uses all data

src/ETHNIC_TRAIN.py

Training based on specified ethnicity
Using SVC model

src/feature_experiment.py

Training based on on specified feature selection results
Using SVC model

src/utils.py

contains various functions for loading, preprocessing, and computing metrics for the manuscript. It also creates data sets based on previously computed features selection.

src/BVFeatureMainCleaned.ipynb

feature tests of getting significant features for Point Biserial, Ftest, Ttest, and Gini and also outputted the heatmaps. Was used to get significant features for each ethnicity.

The folder titled "Results Processing" contains three folders:

Ethnicity Subset Experiment
Feature Test Experiment
Model Experiment

Each folder contains the csv files output from the python experiments code. Also included in each folder are three R scripts:

Tables with CI.R

Used to find the means and confidence interval for each metric within the experiment. Also used to generate figure tables

Plots.R

Script used to generate boxplots for figures

Boxplot Table.R

Used to extract the values of different boxplot components