# MSBA-A7-Public
This is the public repository for the MSBA capstone project of Cohort A Team 7. For the evaluation purpose, we uploaded part of our script here that can be visible by public users.

Our goal in this project is to explore features associated with risks of homeowner insurance and come up with new efficient variables and a precise model that can predict house damage losses at zip code level in the USA. With the help from a fast-growing insurance company, we started from data exploration and found two additional data sources. Then we completed feature engineering and selection. After training the sampke with four models (GLM, Random forest, XGBoost, and Neutal Network), we successfully verified that our new features have a predictive power on insurance loss. Even, some of the variables can add extra power on the existing company model.

In this repository, we uploaded our scripts, summary paper, poster, and presentation slides. 
*As per the request of the company, the scripts for data exploration and feature engineering are excluded.*

Please refer to the index below to locate any files:

`Scripts/`:
* `Sampling.R`: in-time and out-of-time stratified sampling
* `Sampling_Reduced.R`: in-time stratified samling with less observations
* `Feature_Selection_Final.ipynb`: variable selection using Python
* `Variable_Selection_34.R`: extract selected variables from each subset
* `Modeling/`:
   - `GLM/`: 4 scripts
   - `RF/`: 8 scripts
   - `XGBoost/`: 2 scripts
   - `NNModel_prediction.ipynb`: neural network in Python notebook

`Summary Paper.pdf`: project summary paper

`A7 Poster.pdf`: project poster

`A7 Slides.pdf`: presentation slide deck

