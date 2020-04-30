options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)

## - Variable Selection was processed in Python
## - After we selected variables, we used python to export the final train, test, 
##   and out-of-sample validation
## - Here we are going to filter the full_train and full_test (without reducing 0-loss) 
##   and export dataframes with selected variables for modeling.

train_va = read_csv("Scripts/Model_Data/train_va.csv")
dim(train_va)
test_va = read_csv("Scripts/Model_Data/test_va.csv")
dim(test_va)

## Extracted the selected features

train_reduced = read_csv("Scripts/Model_Data/34Var/Train_reduced_34.csv")
colnames(train_reduced)

selectedVar = colnames(train_reduced)

## Train Full
train_full = train_va %>% select(selectedVar)
dim(train_full)
dim(train_reduced)

write_csv(train_full, "Scripts/Model_Data/34Var/Train_full_34.csv")


## Test Full
test_full = test_va %>% select(selectedVar)
dim(test_full)

write_csv(test_full, "Scripts/Model_Data/34Var/Test_full_34.csv")

## Validation
full_df= read_csv("Scripts/Model_Data/full_df.csv")

valida = full_df %>% filter(year %in% c(2014, 2015))

valida_va = valida %>%
  select(-address_id, -pol_id, -state, -city, -zip, - year, -population, -id)

# Save a copy of validation set
write_csv(valida, "Scripts/Model_Data/validation.csv")
write_csv(valida_va, "Scripts/Model_Data/validation_va.csv")

# Validation variables
valid = valida_va %>% select(selectedVar)
dim(valid)

write_csv(valid, "Scripts/Model_Data/34Var/Validation_34.csv")
