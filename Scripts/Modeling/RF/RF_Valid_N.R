options(stringsAsFactors = FALSE)
library(tidyverse)
library(DescTools)
######################################################################random forest_full
library(randomForest)
?randomForest

#set data
train_df  <- read.csv("Final/Final_train_reduced.csv")
test_df <-read.csv("Final/Final_validation.csv")


train_withoutee <- train_df %>% 
  select(-ee, -oldmodel, -ncat)
test_withoutee <- test_df %>% 
  select(-ee, -oldmodel, -ncat)
train_withoutee$annualized_loss <- train_df$ncat/train_df$ee
test_withoutee$annualized_loss <- test_df$ncat/test_df$ee
fit_rf <- randomForest(annualized_loss~.,
                       train_withoutee,
                       ntree=100,
                       do.trace=F)
varImpPlot(fit_rf)

#Predicting our y_hats for train and for test data:
yhat_rf_train <- predict(fit_rf, train_withoutee)
yhat_rf_test <- predict(fit_rf, test_withoutee)

View(yhat_rf_train)
#Calculating MSE for both train and test:
mse_rf_train <- mean((yhat_rf_train - train_withoutee$annualized_loss) ^ 2)
mse_rf_test <- mean((yhat_rf_test - test_withoutee$annualized_loss)^2)
print(mse_rf_train)
print(mse_rf_test)

train_ee3 <- train_df %>% 
  select(ncat, ee)
train_ee3$pred <- yhat_rf_train

test_ee3 <-test_df %>% 
  select(ncat, ee)
test_ee3$pred <- yhat_rf_test
