options(stringAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)
library(splitstackshape)

##### Load Data #####
full_df = read_csv("Scripts/Model_Data/full_df.csv")

##### Randomly select 50% of policies with 0 loss #####

full_df$ncat = ifelse(full_df$ncat > 300000, 300000, full_df$ncat)
full_df <- full_df %>% mutate(level =
                                case_when(
                                  ncat == 0 ~ "no_loss",
                                  ncat >= 11000 ~ "high",
                                  ncat < 11000 & ncat >= 2200 ~ "median",
                                  ncat < 2200 ~ "low"
                                ))
full_df %>% group_by(level) %>% summarise(n = n())

## change to lower case for colnames

## filter 2016,2017,2018
comp_3yrs<-full_df %>% filter(year %in% c(2016,2017,2018))

comp_3yrs_zero = comp_3yrs %>% filter(level == "no_loss")
losses = comp_3yrs %>% filter(level != "no_loss")

set.seed(888)
SAMP<-stratified(comp_3yrs_zero, c("year","state"), size = 0.5, keep.rownames = TRUE, bothSets = TRUE)

train_zero <-as.data.frame(SAMP[1])
nrow(train_zero)
colnames(train_zero) = gsub("SAMP1.", "", colnames(train_zero))
train_zero = train_zero[, -1]

test_zero <- as.data.frame(SAMP[2])
nrow(test_zero)
colnames(test_zero) = gsub("SAMP2.", "", colnames(test_zero))
test_zero = test_zero[, -1]

nrow(train_zero)+nrow(test_zero) ## 189084


###  Merge the 50% no_loss with all losses
reduced_full = rbind(train_zero, losses)
# Check
nrow(reduced_full) == nrow(comp_3yrs) - nrow(test_zero)



#### Stratify again on year, state, level
set.seed(888)
SAMP_R<-stratified(reduced_full, c("year","state","level"), size = 0.7, keep.rownames = TRUE, bothSets = TRUE)
## extract train and test data from the sample
train <-as.data.frame(SAMP_R[1])
test <- as.data.frame(SAMP_R[2])
nrow(train)+nrow(test) == nrow(reduced_full)

colnames(train) = gsub("SAMP1.", "", colnames(train))
colnames(test) = gsub("SAMP2.", "", colnames(test))

train_va = train %>% select(-rn, -address_id, -pol_id, -state, -city, -zip, 
                            - year, -population, -id, -level)

test_va = test %>% select(-rn, -address_id, -pol_id, -state, -city, -zip, 
                            - year, -population, -id, -level)

write_csv(train_va, "Scripts/Model_Data/train_reduced_va.csv")
write_csv(test_va, "Scripts/Model_Data/test_reduced_va.csv")


