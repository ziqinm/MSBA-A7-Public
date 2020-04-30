########XGBOOST with OM

options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)
library(DescTools)
library(xgboost)


### Get started ###
# 1. Train a model setting on TRAIN_FULL, then extract `ncat`, `ee`, `prediction` to a data frame;
# 2. Similar for test datasets: after fitting test to the model, 
#    extract `ncat`, `ee`, `prediction` to ANOTHER data frame;
# 3. Run the followling script to calculate scores;
# 4. Remember to record the AUC scores on the table in drive for EACH model; 
# 5. Once everything is done for one model setting, CLEAN THE ENVIRNMENT and start training another.


### Train the model ### 5 folds & 10 rounds
train_full <- read_csv("selected_34_tr.csv")
dim(train_full)

train_df=train_full

model_df = train_df %>% 
  mutate(ncat_annual=ncat/ee)%>%
  select(-ee,-ncat)
colnames(model_df)

params=model_df$ncat_annual
model_df=model_df%>%
  select(-ncat_annual)

model_df=as.matrix(model_df)

xgb1<- xgboost(data = model_df,
               label = params,
               objective = "reg:squarederror",
               nfold = 5,
               nrounds = 10,
               prediction = TRUE)

importance<-xgb.importance(colnames(model_df), model = xgb1)

### TRAIN_Reduced ###
train_df =train_full

train_df=train_df%>%
  mutate(ncat_annual=ncat/ee) %>%
  select(-ee,-ncat)

train_df=train_df%>%
  select(-ncat_annual)

train_df = as.matrix(train_df)
preds2_train = predict(xgb1, train_df)


model2_train<-train_full%>%
  select(ncat,ee)
model2_train$pred=preds2_train


train_r = model2_train
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
test2<- read.csv("selected_34_te.csv")
dim(test2)

test_df =test2 

test_df=test_df%>%
  mutate(ncat_annual=ncat/ee) %>%
  select(-ee,-ncat)

test_df=test_df%>%
  select(-ncat_annual)

test_df = as.matrix(test_df)
preds2 = predict(xgb1, test_df)


model2<-test2%>%
  select(ncat,ee)
model2$pred=preds2

test_r = model2
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
  labs(title = "XGBoost - TRAIN/TEST Comparation",
       subtitle = "All Variables, With Oldmodel",
       x = "",
       y = "Lorenz") +
  theme_minimal()

# ***SAVE THIS



### TRAIN_Full ###
train_34<- read_csv("Train_full_34.csv")
dim(train_34)

train_full_df =train_34 
  
train_full_df=train_full_df%>%
  mutate(ncat_annual=ncat/ee) %>%
  select(-ee,-ncat)

train_full_df=train_full_df%>%
  select(-ncat_annual)

train_full_df = as.matrix(train_full_df)
preds2_train_34 = predict(xgb1, train_full_df)


model2_tr_34<-train_34%>%
  select(ncat,ee)
model2_tr_34$pred=preds2_train_34

train_f = model2_tr_34
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
test_34<- read_csv("Test_full_34.csv")
dim(test_34)
test_full=test_34

test_full_df =test_34
  
test_full_df=test_full_df%>%
  mutate(ncat_annual=ncat/ee) %>%
  select(-ee,-ncat)

test_full_df=test_full_df%>%
  select(-ncat_annual)

test_full_df = as.matrix(test_full_df)
preds2_test_34 = predict(xgb1, test_full_df)


model2_te_34<-test_34%>%
  select(ncat,ee)
model2_te_34$pred=preds2_test_34

test_f = model2_te_34
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



### VALIDATION ###
valida<- read.csv("Validation_34.csv")
valida$ncat = ifelse(valida$ncat > 300000, 300000, valida$ncat)

vali_d =valida

vali_d=vali_d%>%
  mutate(ncat_annual=ncat/ee) %>%
  select(-ee,-ncat)

vali_d=vali_d%>%
  select(-ncat_annual)

vali_d = as.matrix(vali_d)
preds2_v = predict(xgb1, vali_d)


model2_v<-valida%>%
  select(ncat,ee)
model2_v$pred=preds2_v

valid_df = model2_v
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



save.image(file = modelname.RData)
