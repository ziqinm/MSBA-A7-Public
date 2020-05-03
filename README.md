# MSBA-A7-Public

#### Team Member: Qiaoling Huang, Shihan Li, Ziqin Ma, Chenran Peng, Elmira Ushirova

#### Master of Science in Business Analytics, Questrom School of Business

#### Boston University

----------------------------

This is the public repository for the MSBA capstone project of Cohort A Team 7. For the evaluation purpose, we uploaded part of our work here that is accessable to public users.

Our goal is to explore features associated with risks of homeowner insurance loss, and then build a predictive model, using newly-generated efficient variables, on house damage losses at the zip code level in the United States. With the help from a fast-growing insurance company, we started from data exploration and found two additional data sources. Then we completed feature engineering and selection. After training the sample with four models (GLM, Random forest, XGBoost, and Neutal Network), we successfully verified that our new features have a predictive power on insurance loss. Even, some of the variables can add extra power on the existing company model.

This repository includes modeling samples, partial scripts, and project deliverables. 
*As per the request of the company, the scripts for data exploration and feature engineering are excluded.*

Please refer to the index below to locate files:

`Samples/`: 5 subsets of the data that we used for modeling

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
   - `Model_Eval_NCAT.R`: AUC calculation script
   - `Lorenz Curve Comparision.R`: script for plotting lorenz curves
   - `Lorenz1.png`: Lorenz curve of Oldmodel and XGBoost including Oldmodel as a variable in Train_Full sample
   - `Lorenz2.png`: Lorenz curve of Oldmodel and XGBoost including Oldmodel as a variable in Test_Full sample
   
`Deliverables/`:

* `A7 Poster.pdf`: project poster

* `A7 Slides.pdf`: presentation slide deck (updated on May 1)

* `A7 Summary Paper.pdf`: project summary paper


