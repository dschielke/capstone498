library(tidyverse)
#library(xgboost)
library(magrittr)
library(corrplot)



bbalance <- read_csv("./GitHub/capstone498/Original_data/bureau_balance.csv") 
bureau <- read_csv("./GitHub/capstone498/Original_data/bureau.csv")
cc_balance <- read_csv("./GitHub/capstone498/Original_data/credit_card_balance.csv")
payments <- read_csv("./GitHub/capstone498/Original_data/installments_payments.csv") 
pc_balance <- read_csv("./GitHub/capstone498/Original_data/POS_CASH_balance.csv")
prev <- read_csv("./GitHub/capstone498/Original_data/previous_application.csv")
tr <- read_csv("./GitHub/capstone498/Original_data/application_train.csv") 
te <- read_csv("./GitHub/capstone498/Original_data/application_test.csv")


fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))

sum_bbalance <- bbalance %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_BUREAU) %>% 
  summarise_all(fn) 
rm(bbalance); gc()


str(sum_bureau)
sum_bureau <- bureau %>% 
  left_join(sum_bbalance, by = "SK_ID_BUREAU") %>% 
  select(-SK_ID_BUREAU) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR)
  
  sum_bureau$MONTHS_BALANCE_.args[sum_bureau$MONTHS_BALANCE_.args == 'NULL'] <- FALSE
  sum_bureau$MONTHS_BALANCE_.args[sum_bureau$MONTHS_BALANCE_.args == 'TRUE'] <- TRUE
  sum_bureau$MONTHS_BALANCE_.args <- as.logical(sum_bureau$MONTHS_BALANCE_.args)
  sum_bureau$STATUS_.args[sum_bureau$STATUS_.args == 'NULL'] <- FALSE
  sum_bureau$STATUS_.args[sum_bureau$STATUS_.args == 'TRUE'] <- TRUE
  sum_bureau$STATUS_.args <- as.logical(sum_bureau$STATUS_.args)
  
  sum_bureau <- sum_bureau %>%
  summarise_all(fn)
rm(bureau, sum_bbalance); gc()

sum_cc_balance <- cc_balance %>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
rm(cc_balance); gc()

sum_payments <- payments %>% 
  select(-SK_ID_PREV) %>% 
  #mutate(PAYMENT_PERC = AMT_PAYMENT / AMT_INSTALMENT,
  #       PAYMENT_DIFF = AMT_INSTALMENT - AMT_PAYMENT,
  #       DPD = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
  #       DBD = DAYS_INSTALMENT - DAYS_ENTRY_PAYMENT,
  #       DPD = ifelse(DPD > 0, DPD, 0),
  #       DBD = ifelse(DBD > 0, DBD, 0)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn) 
rm(payments); gc()

sum_pc_balance <- pc_balance %>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)
rm(pc_balance); gc()

sum_prev <- prev %>%
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  #mutate(DAYS_FIRST_DRAWING = ifelse(DAYS_FIRST_DRAWING == 365243, NA, DAYS_FIRST_DRAWING),
  #       DAYS_FIRST_DUE = ifelse(DAYS_FIRST_DUE == 365243, NA, DAYS_FIRST_DUE),
  #       DAYS_LAST_DUE_1ST_VERSION = ifelse(DAYS_LAST_DUE_1ST_VERSION == 365243, NA, DAYS_LAST_DUE_1ST_VERSION),
  #       DAYS_LAST_DUE = ifelse(DAYS_LAST_DUE == 365243, NA, DAYS_LAST_DUE),
  #       DAYS_TERMINATION = ifelse(DAYS_TERMINATION == 365243, NA, DAYS_TERMINATION),
  #       APP_CREDIT_PERC = AMT_APPLICATION / AMT_CREDIT) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn) 
rm(prev); gc()

#tri <- 1:nrow(tr)
#y <- tr$TARGET




tr_joined <- tr %>% 
  #select(-TARGET) %>% 
  #bind_rows(te) %>%
  left_join(sum_bureau, by = "SK_ID_CURR") %>% 
  left_join(sum_cc_balance, by = "SK_ID_CURR") %>% 
  left_join(sum_payments, by = "SK_ID_CURR") %>% 
  left_join(sum_pc_balance, by = "SK_ID_CURR") %>% 
  left_join(sum_prev, by = "SK_ID_CURR") %>% 
  select(-SK_ID_CURR) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) #%>% 
  # mutate(na = apply(., 1, function(x) sum(is.na(x))),
  #        DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED == 365243, NA, DAYS_EMPLOYED),
  #        DAYS_EMPLOYED_PERC = sqrt(DAYS_EMPLOYED / DAYS_BIRTH),
  #        INCOME_CREDIT_PERC = AMT_INCOME_TOTAL / AMT_CREDIT,
  #        INCOME_PER_PERSON = log1p(AMT_INCOME_TOTAL / CNT_FAM_MEMBERS),
  #        ANNUITY_INCOME_PERC = sqrt(AMT_ANNUITY / (1 + AMT_INCOME_TOTAL)),
  #        LOAN_INCOME_RATIO = AMT_CREDIT / AMT_INCOME_TOTAL,
  #        ANNUITY_LENGTH = AMT_CREDIT / AMT_ANNUITY,
  #        CHILDREN_RATIO = CNT_CHILDREN / CNT_FAM_MEMBERS, 
  #        CREDIT_TO_GOODS_RATIO = AMT_CREDIT / AMT_GOODS_PRICE,
  #        INC_PER_CHLD = AMT_INCOME_TOTAL / (1 + CNT_CHILDREN),
  #        SOURCES_PROD = EXT_SOURCE_1 * EXT_SOURCE_2 * EXT_SOURCE_3,
  #        CAR_TO_BIRTH_RATIO = OWN_CAR_AGE / DAYS_BIRTH,
  #        CAR_TO_EMPLOY_RATIO = OWN_CAR_AGE / DAYS_EMPLOYED,
  #        PHONE_TO_BIRTH_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_BIRTH,
  #        PHONE_TO_EMPLOY_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED) 

rm(sum_bbalance,sum_bureau,sum_cc_balance,sum_payments,sum_prev,te,tr); gc()
rm(sum_pc_balance); gc()

#write.csv(tr, file = 'C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/combined_data_train.csv')
#write.csv(te, file = 'C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/combined_data_test.csv')
write.csv(tr_joined, file = 'C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/combined_data_test.csv')


#full null with false
#tr_joined[tr_joined == 'NULL'] <- FALSE


for (column in colnames(tr_joined)) {
  if (grepl('.args',column)) {
    print(column)
    tr_joined[column] <- NULL
  }
}



correlation_matrix <- NULL
correlation_matrix_combined <- NULL
  
#tr_joined %>% 
  #dplyr::select(-city, -year, -weekofyear, -week_start_date) %>%
for (column in colnames(tr_joined)) {
  cor(tr_joined$TARGET, tr_joined[column], use = 'pairwise.complete.obs') -> correlation_matrix
  if (is.na(correlation_matrix)) {}
  else {
    if (correlation_matrix >= 0.05) {
      row.names(correlation_matrix) <- column
      correlation_matrix_combined <- rbind(correlation_matrix_combined,correlation_matrix)
    }
    if (correlation_matrix <= -0.05) {
      row.names(correlation_matrix) <- column
      correlation_matrix_combined <- rbind(correlation_matrix_combined,correlation_matrix)
    }
  }
}
  #cor(tr_joined$TARGET, tr_joined[seq(1,800)], use = 'pairwise.complete.obs') -> correlation_matrix

#correlation_matrix_combined <- as.data.frame(correlation_matrix_combined)

#correlation_matrix_combined <- correlation_matrix_combined[order(-correlation_matrix_combined$TARGET),]

correlation_matrix_combined <- as.matrix(correlation_matrix_combined[order(correlation_matrix_combined[,1],decreasing=TRUE),])

png(height=8000, width=1000, pointsize=15, file='C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/features_corr.png')
corrplot(correlation_matrix_combined, type="lower", method="color",diag=FALSE)
dev.off()

sig_vars <- row.names(correlation_matrix_combined)

tr_joined_sig <- NULL
tr_joined_sig <- tr_joined[sig_vars]


sig_cor_matrix <- cor(tr_joined_sig, use = 'pairwise.complete.obs')
png(height=2000, width=2000, pointsize=10, file='C:/Users/Dan/Desktop/Northwestern/Predict 498 - Summer 2018/sig_features_corr.png')
corrplot(sig_cor_matrix, type="lower", method="color",diag=FALSE)
dev.off()




# 
# 
# 
# docs <- str_subset(names(tr), "FLAG_DOC")
# live <- str_subset(names(tr), "(?!NFLAG_)(?!FLAG_DOC)(?!_FLAG_)FLAG_")
# inc_by_org <- tr_te %>% 
#   group_by(ORGANIZATION_TYPE) %>% 
#   summarise(m = median(AMT_INCOME_TOTAL)) %$% 
#   setNames(as.list(m), ORGANIZATION_TYPE)
# 
# rm(tr, te, fn, sum_bureau, sum_cc_balance, 
#    sum_payments, sum_pc_balance, sum_prev); gc()
# 
# tr_te %<>% 
#   mutate(DOC_IND_KURT = apply(tr_te[, docs], 1, moments::kurtosis),
#          LIVE_IND_SUM = apply(tr_te[, live], 1, sum),
#          NEW_INC_BY_ORG = recode(tr_te$ORGANIZATION_TYPE, !!!inc_by_org),
#          NEW_EXT_SOURCES_MEAN = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, mean),
#          NEW_SCORES_STD = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, sd))%>%
#   mutate_all(funs(ifelse(is.nan(.), NA, .))) %>% 
#   mutate_all(funs(ifelse(is.infinite(.), NA, .))) %>% 
#   data.matrix()