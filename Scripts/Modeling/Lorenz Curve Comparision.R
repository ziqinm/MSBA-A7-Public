## XGBoost
train_f_auc
test_f_auc

## OM

train_om = train_full %>% select(ncat, ee, oldmodel)
colnames(train_om) = c("ncat", "ee", "pred")
train_om = train_om %>% mutate(ncat_a = ncat/ee)
train_om = train_om[order(train_om$pred), ]

sumEE = sum(train_om$ee)    
cutVar = numeric()
for (i in 1:100) {
  cutInd = sumEE * i * 0.01
  cutVar = c(cutVar, cutInd)
}


train_om = train_om %>% mutate(cumu_ee = cumsum(ee))

train_om$group = 0
mi = 1
for (i in 1:100) {
  g = nrow(subset(train_om, cumu_ee <= cutVar[i]))
  train_om$group[mi:g] = i
  mi = g + 1
}

train_om_auc = train_om %>% group_by(group) %>% 
  summarise(cumu_actual = sum(ncat_a))
train_om_auc = train_om_auc %>% mutate(lorenz = cumsum(cumu_actual)/sum(cumu_actual))

B_tr_om = AUC(x = (train_om_auc$group)/100, y = train_om_auc$lorenz)

A_tr_om = 0.5 - B_tr_om
auc_tr_om = (0.5 + A_tr_om)
auc_tr_om  

##### TEST_Full
test_om = test_full %>% select(ncat, ee, oldmodel)
colnames(test_om) = c("ncat", "ee", "pred")
test_om = test_om %>% mutate(ncat_a = ncat/ee)
test_om = test_om[order(test_om$pred), ]

sumEE = sum(test_om$ee)   
cutVar = numeric()
for (i in 1:100) {
  cutInd = sumEE * i * 0.01
  cutVar = c(cutVar, cutInd)
}


test_om = test_om %>% mutate(cumu_ee = cumsum(ee))

test_om$group = 0
mi = 1
for (i in 1:100) {
  g = nrow(subset(test_om, cumu_ee <= cutVar[i]))
  test_om$group[mi:g] = i
  mi = g + 1
}

test_om_auc = test_om %>% group_by(group) %>% 
  summarise(cumu_actual = sum(ncat_a))
test_om_auc = test_om_auc %>% mutate(lorenz = cumsum(cumu_actual)/sum(cumu_actual))

B_te_om = AUC(x = (test_om_auc$group)/100, y = test_om_auc$lorenz)

A_te_om = 0.5 - B_te_om
auc_te_om = (0.5 + A_te_om)
auc_te_om

ggplot() + 
  geom_line(train_f_auc, mapping = aes(x = group, y = lorenz, col = "XGBoost")) +
  geom_line(train_om_auc, mapping = aes(x = group, y = lorenz, col = "Oldmodel")) +
  geom_vline(xintercept = 8) +
  scale_colour_manual("",
                      breaks = c("XGBoost", "Oldmodel"),
                      values = c("navy", "red")) +
  labs(title = "Lorenz Curve: Train_Full",
  	   subtitle = "Oldmodel vs XGBoost with OM") +
  theme_minimal()

ggplot() + 
  geom_line(test_f_auc, mapping = aes(x = group, y = lorenz, col = "XGBoost")) +
  geom_line(test_om_auc, mapping = aes(x = group, y = lorenz, col = "Oldmodel")) +
  geom_vline(xintercept = 8) +
  scale_colour_manual("",
                      breaks = c("XGBoost", "Oldmodel"),
                      values = c("navy", "red")) +
  labs(title = "Lorenz Curve: Test_Full",
  	   subtitle = "Oldmodel vs XGBoost with OM") +
  theme_minimal()