library(tidyverse)
#library(xgboost)
library(magrittr)
library(corrplot)



bbalance <- read_csv("C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/Original_data/bureau_balance.csv") 
bureau <- read_csv("C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/Original_data/bureau.csv")
cc_balance <- read_csv("C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/Original_data/credit_card_balance.csv")
payments <- read_csv("C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/Original_data/installments_payments.csv") 
pc_balance <- read_csv("C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/Original_data/POS_CASH_balance.csv")
prev <- read_csv("C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/Original_data/previous_application.csv")
train <- read_csv("C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/Original_data/application_train.csv") 
test <- read_csv("C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/Original_data/application_test.csv")


fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))

summarized_bureau_balance <- bbalance %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_BUREAU) %>% 
  summarise_all(fn) 
rm(bbalance); gc()


#str(sum_bureau)
summarized_bureau <- bureau %>% 
  left_join(summarized_bbalance, by = "SK_ID_BUREAU") %>% 
  select(-SK_ID_BUREAU) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR)
  
  summarized_bureau$MONTHS_BALANCE_.args[summarized_bureau$MONTHS_BALANCE_.args == 'NULL'] <- FALSE
  summarized_bureau$MONTHS_BALANCE_.args[summarized_bureau$MONTHS_BALANCE_.args == 'TRUE'] <- TRUE
  summarized_bureau$MONTHS_BALANCE_.args <- as.logical(summarized_bureau$MONTHS_BALANCE_.args)
  summarized_bureau$STATUS_.args[summarized_bureau$STATUS_.args == 'NULL'] <- FALSE
  summarized_bureau$STATUS_.args[summarized_bureau$STATUS_.args == 'TRUE'] <- TRUE
  summarized_bureau$STATUS_.args <- as.logical(summarized_bureau$STATUS_.args)
  
  summarized_bureau <- summarized_bureau %>%
  summarise_all(fn)
rm(bureau, sum_bbalance); gc()

summarized_cc_balance <- cc_balance %>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
rm(cc_balance); gc()

summarized_payments <- payments %>% 
  select(-SK_ID_PREV) %>% 
  mutate(PAYMENT_PERC = AMT_PAYMENT / AMT_INSTALMENT,
         PAYMENT_DIFF = AMT_INSTALMENT - AMT_PAYMENT,
         DPD = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
         DBD = DAYS_INSTALMENT - DAYS_ENTRY_PAYMENT,
         DPD = ifelse(DPD > 0, DPD, 0),
         DBD = ifelse(DBD > 0, DBD, 0)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn) 
rm(payments); gc()

summarized_pc_balance <- pc_balance %>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
rm(pc_balance); gc()

summarized_prev <- prev %>%
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  mutate(DAYS_FIRST_DRAWING = ifelse(DAYS_FIRST_DRAWING == 365243, NA, DAYS_FIRST_DRAWING),
         DAYS_FIRST_DUE = ifelse(DAYS_FIRST_DUE == 365243, NA, DAYS_FIRST_DUE),
         DAYS_LAST_DUE_1ST_VERSION = ifelse(DAYS_LAST_DUE_1ST_VERSION == 365243, NA, DAYS_LAST_DUE_1ST_VERSION),
         DAYS_LAST_DUE = ifelse(DAYS_LAST_DUE == 365243, NA, DAYS_LAST_DUE),
         DAYS_TERMINATION = ifelse(DAYS_TERMINATION == 365243, NA, DAYS_TERMINATION),
         APP_CREDIT_PERC = AMT_APPLICATION / AMT_CREDIT) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn) 
rm(prev); gc()

#tri <- 1:nrow(tr)
#y <- tr$TARGET




train_test_summarized_joined <- train %>% 
  #select(-TARGET) %>% 
  bind_rows(test) %>%
  left_join(summarized_bureau, by = "SK_ID_CURR") %>% 
  left_join(summarized_cc_balance, by = "SK_ID_CURR") %>% 
  left_join(summarized_payments, by = "SK_ID_CURR") %>% 
  left_join(summarized_pc_balance, by = "SK_ID_CURR") %>% 
  left_join(summarized_prev, by = "SK_ID_CURR") %>% 
  select(-SK_ID_CURR) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer))


rm(summarized_bbalance,summarized_bureau,summarized_cc_balance,summarized_payments,summarized_pc_balance,summarized_prev,test,train); gc()
saveRDS(train_test_summarized_joined, file = 'C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/train_test_summarized_joined.rds')


#########Code can be quit and continued with the below data import





if (is.null(train_test_summarized_joined)) { 
  train_test_summarized_joined <- readRDS('C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/train_test_summarized_joined.rds')
}


library(stringr)

docs <- str_subset(names(train_test_summarized_joined), "FLAG_DOC")
live <- str_subset(names(train_test_summarized_joined), "(?!NFLAG_)(?!FLAG_DOC)(?!_FLAG_)FLAG_")

train_test_summarized_joined <- train_test_summarized_joined %>% 
   mutate(DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED == 365243, NA, DAYS_EMPLOYED),
          DAYS_EMPLOYED_PERC = sqrt(DAYS_EMPLOYED / DAYS_BIRTH),
          INCOME_CREDIT_PERC = AMT_INCOME_TOTAL / AMT_CREDIT,
          INCOME_PER_PERSON = log1p(AMT_INCOME_TOTAL / CNT_FAM_MEMBERS),
          ANNUITY_INCOME_PERC = sqrt(AMT_ANNUITY / (1 + AMT_INCOME_TOTAL)),
          LOAN_INCOME_RATIO = AMT_CREDIT / AMT_INCOME_TOTAL,
          ANNUITY_LENGTH = AMT_CREDIT / AMT_ANNUITY,
          CHILDREN_RATIO = CNT_CHILDREN / CNT_FAM_MEMBERS, 
          CREDIT_TO_GOODS_RATIO = AMT_CREDIT / AMT_GOODS_PRICE,
          INC_PER_CHLD = AMT_INCOME_TOTAL / (1 + CNT_CHILDREN),
          SOURCES_PROD = EXT_SOURCE_1 * EXT_SOURCE_2 * EXT_SOURCE_3,
          CAR_TO_BIRTH_RATIO = OWN_CAR_AGE / DAYS_BIRTH,
          CAR_TO_EMPLOY_RATIO = OWN_CAR_AGE / DAYS_EMPLOYED,
          PHONE_TO_BIRTH_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_BIRTH,
          PHONE_TO_EMPLOY_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED,
          # Polymonial features for DAYS_EMPLOYED and DAYS_BIRTH
          EMPLOYED_BIRTH_PROD = DAYS_EMPLOYED * DAYS_BIRTH,
          DAYS_EMPLOYED_SQ = DAYS_EMPLOYED**2,
          DAYS_BIRTH_SQ = DAYS_BIRTH**2,
          # Document Count feature
          DOCUMENT_COUNT = FLAG_DOCUMENT_2 +
              FLAG_DOCUMENT_3 +
              FLAG_DOCUMENT_4 +
              FLAG_DOCUMENT_5 +
              FLAG_DOCUMENT_6 +
              FLAG_DOCUMENT_7 +
              FLAG_DOCUMENT_8 +
              FLAG_DOCUMENT_9 +
              FLAG_DOCUMENT_10 +
              FLAG_DOCUMENT_11 +
              FLAG_DOCUMENT_12 +
              FLAG_DOCUMENT_13 +
              FLAG_DOCUMENT_14 +
              FLAG_DOCUMENT_15 +
              FLAG_DOCUMENT_16 +
              FLAG_DOCUMENT_17 +
              FLAG_DOCUMENT_18 +
              FLAG_DOCUMENT_19 +
              FLAG_DOCUMENT_20 +
              FLAG_DOCUMENT_21)

train_test_summarized_joined %<>%
  mutate(DOC_IND_KURT = apply(train_test_summarized_joined[, docs], 1, moments::kurtosis),
         LIVE_IND_SUM = apply(train_test_summarized_joined[, live], 1, sum),
         NEW_EXT_SOURCES_MEAN = apply(train_test_summarized_joined[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, mean),
         NEW_SCORES_STD = apply(train_test_summarized_joined[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, sd))


#fill null with false
#train_test_summarized_joined[train_test_summarized_joined == 'NULL'] <- FALSE


for (column in colnames(train_test_summarized_joined)) {
  if (grepl('.args',column)) {
    print(column)
    train_test_summarized_joined[column] <- NULL
  }
}

correlation_value <- NULL
correlation_matrix <- NULL




train_only <- train_test_summarized_joined[is.na(train_test_summarized_joined$TARGET)]
test_only <- train_test_summarized_joined[!(is.na(train_test_summarized_joined$TARGET))]

for (column in colnames(train_test_summarized_joined)) {
  cor(train_test_summarized_joined$TARGET, train_test_summarized_joined[column], use = 'pairwise.complete.obs') -> correlation_value
  if (is.na(correlation_value)) {}
  else {
    if (correlation_value >= 0.05) {
      row.names(correlation_value) <- column
      correlation_matrix <- rbind(correlation_matrix,correlation_value)
    }
    if (correlation_value <= -0.05) {
      row.names(correlation_value) <- column
      correlation_matrix <- rbind(correlation_matrix,correlation_value)
    }
  }
}

#reorder with highest corr first
correlation_matrix <- as.matrix(correlation_matrix[order(correlation_matrix[,1],decreasing=TRUE),])

sig_vars <- row.names(correlation_matrix)

train_test_summarized_joined_sig <- NULL
train_test_summarized_joined_sig <- train_test_summarized_joined[sig_vars]


sig_cor_matrix <- cor(train_test_summarized_joined_sig, use = 'pairwise.complete.obs')
png(height=2000, width=2000, pointsize=10, file='C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/sig_features_corr.png')
corrplot(sig_cor_matrix, type="lower", method="color",diag=FALSE)
dev.off()


saveRDS(train_test_summarized_joined, file = 'C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/train_test_summarized_joined.rds')
saveRDS(train_test_summarized_joined_sig, file = 'C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/train_test_summarized_joined_sig.rds')

rm(train_test_summarized_joined); gc()

# tr_te %<>% 
#   mutate(DOC_IND_KURT = apply(tr_te[, docs], 1, moments::kurtosis),
#          LIVE_IND_SUM = apply(tr_te[, live], 1, sum),
#          NEW_INC_BY_ORG = recode(tr_te$ORGANIZATION_TYPE, !!!inc_by_org),
#          NEW_EXT_SOURCES_MEAN = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, mean),
#          NEW_SCORES_STD = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, sd))%>%
#   mutate_all(funs(ifelse(is.nan(.), NA, .))) %>% 
#   mutate_all(funs(ifelse(is.infinite(.), NA, .))) %>% 
#   data.matrix()















tr_joined_sig <- readRDS('C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/tr_joined_sig.rds')

str(tr_joined_sig)
for(i in 1:ncol(tr_joined_sig)){
  tr_joined_sig[is.na(tr_joined_sig[,i]), i] <- mean(tr_joined_sig[[i]], na.rm = TRUE)
}

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(tr_joined_sig), size = floor(.75*nrow(tr_joined_sig)), replace = F)
credit_train <- tr_joined_sig[sample, ]
credit_test  <- tr_joined_sig[-sample, ]




################VARIABLE SELECTION###############



library(leaps)
regfit.full=regsubsets(TARGET~.,tr_joined_sig)
summary(regfit.full)
regfit.full=regsubsets(TARGET~.,data=tr_joined_sig,nvmax=5)
reg.summary=summary(regfit.full)







###########Logistic Regression############################################
mae <- function(error) return(mean(abs(error)))

logistic_model <- function(train, test)
{
  form <- "TARGET ~ ." #0.7484
  
  #form <- "TARGET ~ SOURCES_PROD + CREDIT_TO_GOODS_RATIO + DAYS_BIRTH + DAYS_EMPLOYED"
  #form <- "TARGET ~ ."
  #form <- "TARGET ~ ."
  
  
  combined <- rbind(train, test)
  combined_model = glm(formula=form,
                          data = combined,
                          family = "binomial")
  
  return (combined_model)
}

library(MASS)

credit_model <- logistic_model(credit_train, credit_test)
credit_results <-  predict(credit_model, credit_test)
credit_score   <-  mae(credit_test$TARGET - credit_results)

summary(credit_model)
#hist(credit_results)
credit_score

credit_logreg_roc <- roc(credit_test$TARGET,credit_results)
credit_logreg_roc

plot(credit_logreg_roc)
varImp(credit_model)

#########NEURAL NET MODEL################################################

library(caret)

avnnet_model <-  function(train, test) {
  
  combined <- rbind(train, test)
  combined_model <- avNNet(TARGET ~ ., combined, repeats=2, size=2, decay=0.1, linout = TRUE)
  
  return (combined_model)
}

credit_avnnet_model <- avnnet_model(credit_train, credit_test)
credit_avnnet_results <-  predict(credit_avnnet_model, credit_test)#, type = 'raw')
credit_avnnet_score   <-  mae(credit_test$TARGET - credit_avnnet_results)

varImp(credit_avnnet_model)

#fitted <- predict(credit_avnnet_model, credit_test)
summary(credit_avnnet_model)
credit_avnnet_score

hist(credit_avnnet_results)
confusionMatrix(credit_avnnet_results,credit_train$TARGET)

library(pROC)

credit_avnnet_roc <- roc(credit_test$TARGET,credit_avnnet_results)


plot(credit_avnnet_roc)


#############RANDOM FOREST###########################

library(randomForest)

randforest_model <-  function(train, test) {
  
  combined <- rbind(train, test)
  combined_model <- randomForest(TARGET ~ ., combined, ntree=10,keep.forest=TRUE, importance=TRUE)
  
  return (combined_model)
}

credit_randforest_model <- randforest_model(credit_train, credit_test)




randforest_model <-  function(train) {
  
  combined <- train
  combined_model <- randomForest(TARGET ~ ., combined, ntree=10,keep.forest=TRUE, importance=TRUE)
  
  return (combined_model)
}

credit_randforest_model <- randforest_model(credit_train)



credit_randforest_results <-  predict(credit_randforest_model, credit_test)#, type = 'raw')
credit_randforest_score   <-  mae(credit_test$TARGET - credit_randforest_results)

varImp(credit_randforest_model)

summary(credit_randforest_model)
credit_randforest_score

hist(credit_randforest_results)
confusionMatrix(credit_randforest_results,credit_train$TARGET)

library(pROC)

credit_randforest_roc <- roc(credit_test$TARGET,credit_randforest_results)


plot(credit_randforest_roc)



##########BOOSTED TREE###########################################

train <- sj_train_subtrain
test <- sj_train_subtest
y <- train$total_cases

lag_weeks <- seq(1,24,1)

for (week in lag_weeks) {
  train[paste("lagged_total_cases_", week)] <- NULL
}

train$city <- NULL
train$total_cases <- NULL
train$week_start_date <- NULL
train$year <- NULL

for (week in lag_weeks) {
  test[paste("lagged_total_cases_", week)] <- NULL
}

test$city <- NULL
test$total_cases <- NULL
test$week_start_date <- NULL
test$year <- NULL

seed = 1000

xgb_params = list(
  booster="gbtree",
  colsample_bytree= 0.7,
  subsample = 0.7,
  nthread=13,#
  eta = 0.1,
  objective= 'count:poisson',
  max_depth= 4,
  min_child_weight= 1,
  eval_metric= "mae",
  seed = seed
)


#convert test data to an xgbmatrix
dtest <- xgb.DMatrix(data.matrix(test))

#create cross validation folds
kfolds<- 10
folds<-createFolds(y, k = kfolds, list = TRUE, returnTrain = FALSE)
fold <- as.numeric(unlist(folds[1]))

x_train<-train[-fold,]
x_val<-train[fold,]

y_train<-y[-fold]
y_val<-y[fold]


#convert training data to xgbmatrix
dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dval = xgb.DMatrix(as.matrix(x_val), label=y_val)

#perform training
sj_boostedtree_model = xgb.train(params = xgb_params,
                                 data = dtrain,
                                 nrounds = 2000,
                                 watchlist = list(train = dtrain, val=dval),
                                 print_every_n = 25,
                                 early_stopping_rounds=50)

importance_matrix <- xgb.importance(feature_names = colnames(train), model = sj_boostedtree_model)
xgb.plot.importance(importance_matrix = importance_matrix, top_n = 10)


#create xgboost model and get performance stats
dtrain <- xgb.DMatrix(data.matrix(train))
trainpredictions =  predict(sj_boostedtree_model, newdata = dtrain)

plot(sj_train_features$total_cases, type = "l")
lines(trainpredictions, col="blue")

xofy <- seq(801,936,1)

testpredictions =  predict(sj_boostedtree_model, newdata = dtest)
lines(xofy, testpredictions, col="red")

sj_boostedtree_results <-  predict(sj_boostedtree_model, newdata = dtest)
sj_boostedtree_score   <-  mae(sj_train_subtest$total_cases - sj_boostedtree_results)




train <- iq_train_subtrain
test <- iq_train_subtest
y <- train$total_cases

lag_weeks <- seq(1,24,1)

for (week in lag_weeks) {
  train[paste("lagged_total_cases_", week)] <- NULL
}

train$city <- NULL
train$total_cases <- NULL
train$week_start_date <- NULL
train$year <- NULL

for (week in lag_weeks) {
  test[paste("lagged_total_cases_", week)] <- NULL
}

test$city <- NULL
test$total_cases <- NULL
test$week_start_date <- NULL
test$year <- NULL

seed = 1000

xgb_params = list(
  booster="gbtree",
  colsample_bytree= 0.7,
  subsample = 0.7,
  nthread=13,#
  eta = 0.1,
  objective= 'count:poisson',
  max_depth= 4,
  min_child_weight= 1,
  eval_metric= "mae",
  seed = seed
)


#convert test data to an xgbmatrix
dtest <- xgb.DMatrix(data.matrix(test))

#create cross validation folds
kfolds<- 10
folds<-createFolds(y, k = kfolds, list = TRUE, returnTrain = FALSE)
fold <- as.numeric(unlist(folds[1]))

x_train<-train[-fold,]
x_val<-train[fold,]

y_train<-y[-fold]
y_val<-y[fold]


#convert training data to xgbmatrix
dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dval = xgb.DMatrix(as.matrix(x_val), label=y_val)

#perform training
iq_boostedtree_model = xgb.train(params = xgb_params,
                                 data = dtrain,
                                 nrounds = 2000,
                                 watchlist = list(train = dtrain, val=dval),
                                 print_every_n = 25,
                                 early_stopping_rounds=50)

importance_matrix <- xgb.importance(feature_names = colnames(train), model = iq_boostedtree_model)
xgb.plot.importance(importance_matrix = importance_matrix, top_n = 10)


#create xgboost model and get performance stats
dtrain <- xgb.DMatrix(data.matrix(train))
trainpredictions =  predict(iq_boostedtree_model, newdata = dtrain)

plot(iq_train_features$total_cases, type = "l")
lines(trainpredictions, col="blue")

xofy <- seq(401,520,1)

testpredictions =  predict(iq_boostedtree_model, newdata = dtest)
lines(xofy, testpredictions, col="red")

iq_boostedtree_results <-  predict(iq_boostedtree_model, newdata = dtest)
iq_boostedtree_score   <-  mae(iq_train_subtest$total_cases - iq_boostedtree_results)



#Make our csv files for submission

test_features = read.csv('.../dengue_features_test.csv')

#repeat our data treatments we did ont he training set
veg_index_fill <- as.data.frame(cbind(test_features$ndvi_ne, test_features$ndvi_nw, test_features$ndvi_se, test_features$ndvi_sw))
veg_index_fill[is.na(veg_index_fill)] <- 0
veg_index_fill$total <- veg_index_fill$V1 + veg_index_fill$V2 + veg_index_fill$V3 + veg_index_fill$V4
test_features$total_ndvi <- veg_index_fill$total

#split the data by city
sj_test_features = test_features %>% filter(city == 'sj')
iq_test_features = test_features %>% filter(city == 'iq')


#add 8 wk lagged temp variable
eightwklag <- c(NA,NA,NA,NA,NA,NA,NA,NA)
eightwklag <- append(eightwklag, sj_test_features$reanalysis_air_temp_k)
eightwklag <- head(eightwklag, (length(eightwklag) - 8))

sj_test_features$eight_week_lag_temp <- eightwklag

eightwklag <- c(NA,NA,NA,NA,NA,NA,NA,NA)
eightwklag <- append(eightwklag, iq_test_features$reanalysis_air_temp_k)
eightwklag <- head(eightwklag, (length(eightwklag) - 8))

iq_test_features$eight_week_lag_temp <- eightwklag

#fill NAs
features = c("reanalysis_dew_point_temp_k", 
             "reanalysis_tdtr_k",
             "reanalysis_specific_humidity_g_per_kg",
             "eight_week_lag_temp")

sj_test_features[features] %<>% na.locf(fromLast = TRUE)
iq_test_features[features] %<>% na.locf(fromLast = TRUE) 

sj_test_features$negbin_predicted <- predict(sj_negbin_model,sj_test_features)
iq_test_features$negbin_predicted <- predict(iq_negbin_model,iq_test_features)

submissions = read.csv('.../submission_format.csv')
inner_join(submissions, rbind(sj_test_features,iq_test_features)) %>%
  dplyr::select(city, year, weekofyear, total_cases = negbin_predicted) ->
  negbin_predictions

negbin_predictions$total_cases %<>% round()
write.csv(negbin_predictions, '.../negbin_predictions.csv', row.names = FALSE)


sj_test_features$avnnet_predicted <- predict(sj_avnnet_model,sj_test_features)
iq_test_features$avnnet_predicted <- predict(iq_avnnet_model,iq_test_features)

inner_join(submissions, rbind(sj_test_features,iq_test_features)) %>%
  dplyr::select(city, year, weekofyear, total_cases = avnnet_predicted) ->
  avnnet_predictions

avnnet_predictions$total_cases %<>% round()
write.csv(avnnet_predictions, '.../avnnet_predictions.csv', row.names = FALSE)


sj_test_features$boostedtree_predicted <- predict(sj_boostedtree_model, newdata = xgb.DMatrix(data.matrix(sj_test_features)))
iq_test_features$boostedtree_predicted <- predict(iq_boostedtree_model, newdata = xgb.DMatrix(data.matrix(iq_test_features)))

inner_join(submissions, rbind(sj_test_features,iq_test_features)) %>%
  dplyr::select(city, year, weekofyear, total_cases = boostedtree_predicted) ->
  boostedtree_predictions

boostedtree_predictions$total_cases %<>% round()
write.csv(boostedtree_predictions, '.../boostedtree_predictions.csv', row.names = FALSE)







