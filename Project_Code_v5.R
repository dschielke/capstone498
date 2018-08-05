library(tidyverse)
library(magrittr)
library(corrplot)
library(caret)

#To skip table joining, start at line 110
#To skip to modelling, start at line 252

bbalance <- read_csv("C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/Original_data/bureau_balance.csv") 
bureau <- read_csv("C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/Original_data/bureau.csv")
cc_balance <- read_csv("C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/Original_data/credit_card_balance.csv")
payments <- read_csv("C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/Original_data/installments_payments.csv") 
pc_balance <- read_csv("C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/Original_data/POS_CASH_balance.csv")
prev <- read_csv("C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/Original_data/previous_application.csv")
train <- read_csv("C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/Original_data/application_train.csv") 
test <- read_csv("C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/Original_data/application_test.csv")


fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))

summarized_bbalance <- bbalance %>%
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
rm(bureau, summarized_bbalance); gc()

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

#pause execution here or earlier if running on a commodity PC/laptop to prevent session crashes

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
  #select(-SK_ID_CURR) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer))


rm(summarized_bbalance,summarized_bureau,summarized_cc_balance,summarized_payments,summarized_pc_balance,summarized_prev,test,train); gc()
saveRDS(train_test_summarized_joined, file = 'C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/train_test_summarized_joined.rds')




#############################################################################
######### Analysis code can be started with the below data import #########
#############################################################################




if (!(exists("train_test_summarized_joined"))) { 
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


#the features below take a long time to compute and didn't add any value

#train_test_summarized_joined %<>%
#  mutate(#DOC_IND_KURT = apply(train_test_summarized_joined[, docs], 1, moments::kurtosis),
#         #LIVE_IND_SUM = apply(train_test_summarized_joined[, live], 1, sum),
#         NEW_EXT_SOURCES_MEAN = apply(train_test_summarized_joined[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, mean),
#         NEW_SCORES_STD = apply(train_test_summarized_joined[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, sd))
#   mutate_all(funs(ifelse(is.nan(.), NA, .))) %>% 
#   mutate_all(funs(ifelse(is.infinite(.), NA, .))) 

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




train_only <- train_test_summarized_joined[!(is.na(train_test_summarized_joined$TARGET)),]
test_only <- train_test_summarized_joined[is.na(train_test_summarized_joined$TARGET),]

for (column in colnames(train_test_summarized_joined)) {
  cor(train_only$TARGET, train_only[column], use = 'pairwise.complete.obs') -> correlation_value
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
sig_vars <- append("SK_ID_CURR",sig_vars)

train_test_summarized_joined_sig <- NULL
train_test_summarized_joined_sig <- train_test_summarized_joined[sig_vars]

train_test_summarized_joined_sig_noID <- train_test_summarized_joined_sig %>% 
  select(-SK_ID_CURR)


sig_cor_matrix <- cor(train_test_summarized_joined_sig_noID, use = 'pairwise.complete.obs')
png(height=2000, width=2000, pointsize=10, file='C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/sig_features_corr.png')
corrplot(sig_cor_matrix, type="lower", method="color",diag=FALSE)
dev.off()


saveRDS(train_test_summarized_joined, file = 'C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/train_test_summarized_joined_all_feats.rds')
saveRDS(train_test_summarized_joined_sig, file = 'C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/train_test_summarized_joined_sig.rds')

rm(train_test_summarized_joined, train_test_summarized_joined_sig_noID, train_only, test_only); gc()













##################### BEGIN MODELING ########################

#Re import data and split out to training and test sets now that we have all our transforms finalized
train_test_sig_feats <- readRDS('C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/train_test_summarized_joined_sig.rds')

#str(train_test_sig_feats)

non_id_cols <- colnames(train_test_sig_feats)
remove <- c("SK_ID_CURR","TARGET")
non_id_cols <- non_id_cols[!non_id_cols %in% remove]

#fill all NAs with column mean
for(column in non_id_cols){
  train_test_sig_feats[is.na(train_test_sig_feats[,column]), column] <- mean(train_test_sig_feats[[column]], na.rm = TRUE)
}

head(train_test_sig_feats)

#library(tidyr)
#library(ggplot2)
#check the results of imputation...
#ggplot(gather(train_test_sig_feats), aes(value)) + 
#  geom_histogram() + 
#  facet_wrap(~key, scales = 'free_x')

train_only <- train_test_sig_feats[!(is.na(train_test_sig_feats$TARGET)),]
test_only <- train_test_sig_feats[is.na(train_test_sig_feats$TARGET),]

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(train_only), size = floor(.75*nrow(train_only)), replace = F)
credit_train <- train_only[sample, ]
credit_test  <- train_only[-sample, ]

mae <- function(error) return(mean(abs(error)))


################VARIABLE SELECTION###############

credit_train_sliced <- credit_train[,c(0,1,2,3,4)]

library(leaps)
regfit.full=regsubsets(TARGET~.,credit_train,nvmax=9,method="forward")# really.big = TRUE)
options(max.print = 11)
summary(regfit.full)

reg.summary=summary(regfit.full)
summary.dataframe <- as.data.frame(reg.summary$outmat)

write.csv(summary.dataframe, file= "C://Users//Dan//Desktop//Northwestern//Predict 498 - Summer 2018//forward.csv")
#names(reg.summary)
reg.summary$rsq
reg.summary$cp
reg.summary$bic
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")


coef(regfit.full,21)


regfit.full=regsubsets(TARGET~.,credit_train,nvmax=9,method="backward")# really.big = TRUE)
summary(regfit.full)

reg.summary=summary(regfit.full)
summary.dataframe <- as.data.frame(reg.summary$outmat)

write.csv(summary.dataframe, file= "C://Users//Dan//Desktop//Northwestern//Predict 498 - Summer 2018//backward.csv")

reg.summary$rsq
reg.summary$cp
reg.summary$bic
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")


coef(regfit.full,21)



regfit.full=regsubsets(TARGET~.,credit_train,nvmax=9,method="seqrep")# really.big = TRUE)
summary(regfit.full)

reg.summary=summary(regfit.full)
summary.dataframe <- as.data.frame(reg.summary$outmat)

write.csv(summary.dataframe, file= "C://Users//Dan//Desktop//Northwestern//Predict 498 - Summer 2018//stepwise.csv")

reg.summary$rsq
reg.summary$cp
reg.summary$bic
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")

coef(regfit.full,21)





###########Logistic Regression############################################


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
library(pROC)

credit_model <- logistic_model(credit_train, credit_test)
credit_results <-  predict(credit_model, credit_test)
credit_score   <-  mae(credit_test$TARGET - credit_results)

summary(credit_model)
#hist(credit_results)
credit_score

credit_logreg_roc <- roc(credit_test$TARGET,credit_results) #0.7486

plot(credit_logreg_roc)
var_imp.dataframe <- as.data.frame(varImp(credit_model))
write.csv(var_imp.dataframe, file= "C://Users//Dan//Desktop//Northwestern//Predict 498 - Summer 2018//logistic.csv")


#########NEURAL NET MODEL################################################

library(caret)

avnnet_model <-  function(train, test) {
  
  combined <- rbind(train, test)
  combined_model <- avNNet(TARGET ~ EXT_SOURCE_2 +
                             EXT_SOURCE_3 +
                             EXT_SOURCE_1 +
                             SOURCES_PROD +
                             CREDIT_TO_GOODS_RATIO +
                             DAYS_BIRTH +
                             EMPLOYED_BIRTH_PROD +
                             DAYS_ID_PUBLISH +
                             DAYS_INSTALMENT_sd +
                             NAME_EDUCATION_TYPE +
                             NAME_CONTRACT_STATUS_mean +
                             DAYS_CREDIT_mean +
                             MONTHS_BALANCE_sd.y +
                             CREDIT_ACTIVE_mean +
                             DAYS_ENTRY_PAYMENT_sd +
                             DAYS_LAST_PHONE_CHANGE, combined, repeats=25, size=1.1, decay=1, linout = FALSE) #
  
  
  return (combined_model)
}

credit_avnnet_model <- avnnet_model(credit_train, credit_test)
credit_avnnet_results <-  predict(credit_avnnet_model, credit_test)#, type = 'raw')
credit_avnnet_score   <-  mae(credit_test$TARGET - credit_avnnet_results)

#varImp(credit_avnnet_model)
#var_imp.dataframe <- as.data.frame(varImp(credit_avnnet_model))
#write.csv(var_imp.dataframe, file= "C://Users//Dan//Desktop//Northwestern//Predict 498 - Summer 2018//avnnet.csv")

credit_avnnet_model$model$wts

summary(credit_avnnet_model)
credit_avnnet_score

library(pROC)

credit_avnnet_roc <- roc(credit_test$TARGET,credit_avnnet_results) #0.5514


plot(credit_avnnet_roc)





#############RANDOM FOREST###########################

library(randomForest)

randforest_model <-  function(train, test) {
  
  combined <- rbind(train, test)
  #adjusted the below line to use "train" only to prevent overfitting
  #combined_model <- randomForest(TARGET ~ ., train, ntree=100,keep.forest=TRUE, importance=TRUE,do.trace = 10)
  combined_model <- randomForest(TARGET ~ EXT_SOURCE_2 +
                                 EXT_SOURCE_3 +
                                 EXT_SOURCE_1 +
                                 SOURCES_PROD +
                                 CREDIT_TO_GOODS_RATIO +
                                 DAYS_BIRTH +
                                 EMPLOYED_BIRTH_PROD +
                                 DAYS_ID_PUBLISH +
                                 DAYS_INSTALMENT_sd +
                                 NAME_EDUCATION_TYPE +
                                 NAME_CONTRACT_STATUS_mean +
                                 DAYS_CREDIT_mean +
                                 MONTHS_BALANCE_sd.y +
                                 CREDIT_ACTIVE_mean +
                                 DAYS_ENTRY_PAYMENT_sd +
                                 DAYS_LAST_PHONE_CHANGE, train, ntree=100, nodesize = 100,keep.forest=TRUE, importance=TRUE,do.trace = 10)
  
  
  return (combined_model)
}

credit_randforest_model <- randforest_model(credit_train, credit_test)

credit_randforest_results <-  predict(credit_randforest_model, credit_test)#, type = 'raw')
credit_randforest_score   <-  mae(credit_test$TARGET - credit_randforest_results)

varImp(credit_randforest_model)
var_imp.dataframe <- as.data.frame(varImp(credit_randforest_model))
write.csv(var_imp.dataframe, file= "C://Users//Dan//Desktop//Northwestern//Predict 498 - Summer 2018//random_forest.csv")


summary(credit_randforest_model)
credit_randforest_score


library(pROC)

credit_randforest_roc <- roc(credit_test$TARGET,credit_randforest_results) #0.7382


plot(credit_randforest_roc)




##########BOOSTED TREE###########################################

library(xgboost)
# convert to matrix for XGBoost
credit_train_copy <- credit_train
credit_train_copy$TARGET <- NULL
credit_train_matrix = data.matrix(credit_train_copy)
credit_test_copy <- credit_test
credit_test_copy$TARGET <- NULL
credit_test_matrix = data.matrix(credit_test_copy)


#---------------------------
cat("Preparing data...\n")

dtrain <- xgb.DMatrix(data = credit_train_matrix, label = credit_train$TARGET)
dval <- xgb.DMatrix(data = credit_test_matrix, label = credit_test$TARGET)
cols <- colnames(credit_train_matrix)

#---------------------------
cat("Training model...\n")

p <- list(objective = "binary:logistic",
          booster = "gbtree",
          eval_metric = "auc",
          nthread = 6,
          eta = 0.025,
          max_depth = 6,
          min_child_weight = 19,
          gamma = 0,
          subsample = 0.8,
          colsample_bytree = 0.632,
          alpha = 0,
          lambda = 0.05,
          nrounds = 1000)

m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 50, early_stopping_rounds = 200)


credit_xgboost_results <-  predict(m_xgb, dval)#, type = 'raw')
credit_xgboost_score   <-  mae(credit_test$TARGET - credit_xgboost_results)



xgb.importance(cols, model=m_xgb)
var_imp.dataframe <- as.data.frame(xgb.importance(cols, model=m_xgb))
write.csv(var_imp.dataframe, file= "C://Users//Dan//Desktop//Northwestern//Predict 498 - Summer 2018//xgboost.csv")


summary(m_xgb)
credit_xgboost_score


library(pROC)

credit_xgboost_roc <- roc(credit_test$TARGET,credit_xgboost_results) #0.7596


plot(credit_xgboost_roc)


