options(stringAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)

##### Load Data #####
comp = read_csv("Datasets/bu_class_202002.csv")
colnames(comp) = tolower(colnames(comp))
comp$city = tolower(comp$city)
crim_NV = read_csv("Scripts/Model_Data/Crime_NewVar_5S.csv")
lgt_NV = read_csv("Scripts/Model_Data/Lgt_NewVar_5S.csv")

#### Join everything together
full_df = left_join(comp, crim_NV, by = c("state", "city"))
full_df = left_join(full_df, lgt_NV, by = "zip")
colnames(full_df)[3] = "state"
full_df = full_df[, -c(216, 217)]
sum(complete.cases(full_df)) == nrow(full_df)

full_df$id = 1:nrow(full_df)

#### Convert scale of variables

### population to thousand population
full_df$population = full_df$population/1000
pop_names = grep("pop_", names(full_df), value = TRUE)
full_df[, pop_names] = full_df[, pop_names] * 1000

### total area: square-meter to square-mile
## 1 sqm = 0.0000003861 sqml
## 2589988.11
area_names = grep("sq", names(full_df), value = TRUE)
full_df[, area_names] = full_df[, area_names]*2589988.11

write_csv(full_df, "Scripts/Model_Data/full_df.csv")

##### Sampling #####

library(splitstackshape)

##### Capping the company data #####

full_df$ncat = ifelse(full_df$ncat > 300000, 300000, full_df$ncat)
full_df <- full_df %>% mutate(level =
                                        case_when(
                                          ncat == 0 ~ "no_loss",
                                          ncat >= 11000 ~ "high",
                                          ncat < 11000 & ncat >= 2200 ~ "median",
                                          ncat < 2200 ~ "low"
                                        ))
full_df %>% group_by(level) %>% summarise(n = n())


## filter 2016,2017,2018
comp_3yrs<-full_df %>% filter(year %in% c(2016,2017,2018))

## stratified sample
SAMP<-stratified(comp_3yrs, c("year","state","level"), size = 0.7, keep.rownames = TRUE, bothSets = TRUE)
class(SAMP)

## extract train and test data from the sample
train <-as.data.frame(SAMP[1])
test <- as.data.frame(SAMP[2])
## check the total row
nrow(train)+nrow(test) ## 189084

colnames(train) = gsub("SAMP1.", "", colnames(train))
colnames(test) = gsub("SAMP2.", "", colnames(test))

write_csv(train, "Scripts/Model_Data/train.csv")
write_csv(test, "Scripts/Model_Data/test.csv")

train_va = train %>% select(-rn, -address_id, -pol_id, -state, -city, -zip, 
                            - year, -population, -id, -level)

test_va = test %>% select(-rn, -address_id, -pol_id, -state, -city, -zip, 
                          - year, -population, -id, -level)

write_csv(train_va, "Scripts/Model_Data/train_va.csv")
write_csv(test_va, "Scripts/Model_Data/test_va.csv")
