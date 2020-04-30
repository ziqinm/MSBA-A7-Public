options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)
library(DescTools)

### Load Data ###

train_reduced = read_csv("Scripts/Model_Data/34Var/Train_reduced_34.csv")
test_reduced = read_csv("Scripts/Model_Data/34Var/Test_reduced_34.csv")
train_full = read_csv("Scripts/Model_Data/34Var/Train_full_34.csv")
test_full = read_csv("Scripts/Model_Data/34Var/Test_full_34.csv")
valida = read_csv("Scripts/Model_Data/34Var/Validation_34.csv")
valida$ncat = ifelse(valida$ncat > 300000, 300000, valida$ncat)

### Setting 4 ###
## Model: GLM
## Data: crim + lgt
## Distribution: Poisson
## Oldmodel: with


# Create annualized ncat as the Y

trainR = train_reduced
model_df = trainR %>% mutate(ncat_annual = ncat/ee) %>% 
  # then remove ee and ncat
  select(-ee, -ncat)
colnames(model_df)

glm4 = glm(ncat_annual ~ ., data = model_df, family = poisson())
summary(glm4)
pred_trainR = predict(glm4)

# Save ncat, ee, pred as a dataframe for AUC
glm4_trainR = data.frame(ncat = trainR$ncat,
                        ee = trainR$ee,
                        pred = pred_trainR)



### TRAIN_Reduced ###

train_r = glm4_trainR
colnames(train_r) = c("ncat", "ee", "pred")

train_r = train_r %>% mutate(ncat_a = ncat/ee)
train_r = train_r[order(train_r$pred), ]

sumEE = sum(train_r$ee) 
cutVar = numeric()
for (i in 1:100) {
  cutInd = sumEE * i * 0.01
  cutVar = c(cutVar, cutInd)
}

train_r = train_r %>% mutate(cumu_ee = cumsum(ee))

train_r$group = 0
mi = 1
for (i in 1:100) {
  g = nrow(subset(train_r, cumu_ee <= cutVar[i]))
  train_r$group[mi:g] = i
  mi = g + 1
}

train_r_auc = train_r %>% group_by(group) %>% 
  summarise(cumu_actual = sum(ncat_a))
train_r_auc = train_r_auc %>% mutate(lorenz = cumsum(cumu_actual)/sum(cumu_actual))

B_tr_r = AUC(x = (train_r_auc$group)/100, y = train_r_auc$lorenz)
A_tr_r = 0.5 - B_tr_r
auc_tr_r = (0.5 + A_tr_r)
auc_tr_r

### TEST_Reduced ###
testR = test_reduced
testR = testR %>% mutate(ncat_annual = ncat/ee) %>% 
  select(-ee, -ncat)

pred_testR = predict(glm4, newdata = testR)

glm4_testR = data.frame(ncat = test_reduced$ncat,
                       ee = test_reduced$ee,
                       pred = pred_testR)

test_r = glm4_testR
colnames(test_r) = c("ncat", "ee", "pred")

test_r = test_r %>% mutate(ncat_a = ncat/ee)
test_r = test_r[order(test_r$pred), ]

sumEE = sum(test_r$ee)   
cutVar = numeric()
for (i in 1:100) {
  cutInd = sumEE * i * 0.01
  cutVar = c(cutVar, cutInd)
}

test_r = test_r %>% mutate(cumu_ee = cumsum(ee))

test_r$group = 0
mi = 1
for (i in 1:100) {
  g = nrow(subset(test_r, cumu_ee <= cutVar[i]))
  test_r$group[mi:g] = i
  mi = g + 1
}

test_r_auc = test_r %>% group_by(group) %>% 
  summarise(cumu_actual = sum(ncat_a))
test_r_auc = test_r_auc %>% mutate(lorenz = cumsum(cumu_actual)/sum(cumu_actual))

B_te_r = AUC(x = (test_r_auc$group)/100, y = test_r_auc$lorenz)
A_te_r = 0.5 - B_te_r
auc_te_r = (0.5 + A_te_r)
auc_te_r  


### Train/Test Comparison ###
## CHECK OVERFITTING!

plot_df = data.frame(group = train_r_auc$group,
                     train_pred = train_r_auc$lorenz,
                     test_pred = test_r_auc$lorenz)

ggplot(plot_df) +
  geom_line(aes(x = factor(group), y = train_pred, group = 1, col = "Train")) +
  geom_line(aes(x = factor(group), y = test_pred, group = 1, col = "Test")) +
  scale_colour_manual("",
                      breaks = c("Train", "Test"),
                      values = c("red", "navy")) +
  labs(title = "Train/Test Lorenz Curve",
       subtitle = "GLM, 34 variables, Poisson distribution, with OM",
       x = "",
       y = "Lorenz") +
  theme_minimal()

# ***SAVE THIS

### TRAIN_Full ###

trainF = train_full

trainF = trainF %>% mutate(ncat_annual = ncat/ee) %>% 
  # then remove `ee` and `ncat`
  select(-ee, -ncat)

pred_trainF = predict(glm4, newdata = trainF)
pred_trainF = ifelse(pred_trainF < 0, 0, pred_trainF)

glm4_trainF = data.frame(ncat = train_full$ncat,
                          ee = train_full$ee,
                          pred = pred_trainF)

### TRAIN_Full ###

train_f = glm4_trainF
colnames(train_f) = c("ncat", "ee", "pred")

train_f = train_f %>% mutate(ncat_a = ncat/ee)
train_f = train_f[order(train_f$pred), ]

sumEE = sum(train_f$ee)  
cutVar = numeric()
for (i in 1:100) {
  cutInd = sumEE * i * 0.01
  cutVar = c(cutVar, cutInd)
}

train_f = train_f %>% mutate(cumu_ee = cumsum(ee))

train_f$group = 0
mi = 1
for (i in 1:100) {
  g = nrow(subset(train_f, cumu_ee <= cutVar[i]))
  train_f$group[mi:g] = i
  mi = g + 1
}

train_f_auc = train_f %>% group_by(group) %>% 
  summarise(cumu_actual = sum(ncat_a))
train_f_auc = train_f_auc %>% mutate(lorenz = cumsum(cumu_actual)/sum(cumu_actual))

B_tr_f = AUC(x = (train_f_auc$group)/100, y = train_f_auc$lorenz)
A_tr_f = 0.5 - B_tr_f
auc_tr_f = (0.5 + A_tr_f)
auc_tr_f



### TEST_Full ###

testF = test_full

testF = testF %>% mutate(ncat_annual = ncat/ee) %>% 
  # then remove `ee` and `ncat`
  select(-ee, -ncat)

pred_testF = predict(glm4, newdata = testF)

glm4_testF = data.frame(ncat = test_full$ncat,
                         ee = test_full$ee,
                         pred = pred_testF)

### TEST_Full ###

test_f = glm4_testF
colnames(test_f) = c("ncat", "ee", "pred")

test_f = test_f %>% mutate(ncat_a = ncat/ee)
test_f = test_f[order(test_f$pred), ]

sumEE = sum(test_f$ee)  
cutVar = numeric()
for (i in 1:100) {
  cutInd = sumEE * i * 0.01
  cutVar = c(cutVar, cutInd)
}

test_f = test_f %>% mutate(cumu_ee = cumsum(ee))

test_f$group = 0
mi = 1
for (i in 1:100) {
  g = nrow(subset(test_f, cumu_ee <= cutVar[i]))
  test_f$group[mi:g] = i
  mi = g + 1
}

test_f_auc = test_f %>% group_by(group) %>% 
  summarise(cumu_actual = sum(ncat_a))
test_f_auc = test_f_auc %>% mutate(lorenz = cumsum(cumu_actual)/sum(cumu_actual))

B_te_f = AUC(x = (test_f_auc$group)/100, y = test_f_auc$lorenz)
A_te_f = 0.5 - B_te_f
auc_te_f = (0.5 + A_te_f)
auc_te_f


### Validation ###

valid = valida

valid = valid %>% mutate(ncat_annual = ncat/ee) %>% 
  # then remove `ee` and `ncat`
  select(-ee, -ncat)

pred_valid = predict(glm4, newdata = valid)

glm4_valid = data.frame(ncat = valida$ncat,
                        ee = valida$ee,
                        pred = pred_valid)

valid_df = glm4_valid
colnames(valid_df) = c("ncat", "ee", "pred")

valid_df = valid_df %>% mutate(ncat_a = ncat/ee)
valid_df = valid_df[order(valid_df$pred), ]

sumEE = sum(valid_df$ee) 
cutVar = numeric()
for (i in 1:100) {
  cutInd = sumEE * i * 0.01
  cutVar = c(cutVar, cutInd)
}

valid_df = valid_df %>% mutate(cumu_ee = cumsum(ee))

valid_df$group = 0
mi = 1
for (i in 1:100) {
  g = nrow(subset(valid_df, cumu_ee <= cutVar[i]))
  valid_df$group[mi:g] = i
  mi = g + 1
}

valid_auc = valid_df %>% group_by(group) %>% 
  summarise(cumu_actual = sum(ncat_a))
valid_auc = valid_auc %>% mutate(lorenz = cumsum(cumu_actual)/sum(cumu_actual))

B_v = AUC(x = (valid_auc$group)/100, y = valid_auc$lorenz)
A_v = 0.5 - B_v
auc_v = (0.5 + A_v)
auc_v
