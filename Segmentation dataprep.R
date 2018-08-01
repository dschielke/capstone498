rm(list=ls())

setwd("C:/Users/Cammy/Documents/NW/498/data")

library(tidyverse)
library(magrittr)
library(reshape2)

###########Processing Credit Card Loan Info#############

#Take most recent status of each previous loan
cc_balance     = read_csv("input/credit_card_balance.csv")
cc_balance     = cc_balance[order(cc_balance$SK_ID_CURR,cc_balance$SK_ID_PREV,-cc_balance$MONTHS_BALANCE),]
cc_balance$dup = duplicated(cc_balance[,c("SK_ID_CURR","SK_ID_PREV")])
cc_unq         = cc_balance[cc_balance$dup == F,c("SK_ID_CURR","SK_ID_PREV","NAME_CONTRACT_STATUS")]
cc_unq1        = dcast(cc_unq,SK_ID_CURR~NAME_CONTRACT_STATUS)

colnames(cc_unq1)[2:ncol(cc_unq1)]=c(paste0("cc_num_",colnames(cc_unq1)[2:ncol(cc_unq1)]))

#Summarize credit card history and balance details by customer
cc_dets = 
  cc_balance %>%
  filter(NAME_CONTRACT_STATUS=="Active") %>%
  group_by(SK_ID_CURR) %>%
  summarise(
    cc_mos_since_start       = min(MONTHS_BALANCE),
    cc_mos_since_end         = max(MONTHS_BALANCE),
    cc_mos_past_due          = sum(SK_DPD > 0),
    cc_mos_since_lastbalance = max(ifelse(AMT_BALANCE>0,MONTHS_BALANCE,-99999)),
    cc_avg_balanace          = mean(AMT_BALANCE),
    cc_avg_payment           = mean(AMT_PAYMENT_TOTAL_CURRENT),
    cc_avg_num_drawings      = mean(CNT_DRAWINGS_CURRENT),
    cc_avg_num_atm_drawings  = mean(CNT_DRAWINGS_ATM_CURRENT)
  )

cc_unq1 = merge(cc_unq1,cc_dets  ,by="SK_ID_CURR",all.x=T)
cc_unq1$cc_mos_since_lastbalance[cc_unq1$cc_mos_since_lastbalance==-99999]  = NA

#View(cc_unq1)
rm(cc_balance,cc_unq,cc_dets)
gc()


###########Processing POS Loan Info#############

#Take most recent status of each previous loan
pc_balance     = read_csv("input/POS_CASH_balance.csv")
pc_balance     = pc_balance[order(pc_balance$SK_ID_CURR,pc_balance$SK_ID_PREV,-pc_balance$MONTHS_BALANCE),]
pc_balance$dup = duplicated(pc_balance[,c("SK_ID_CURR","SK_ID_PREV")])
pc_unq         = pc_balance[pc_balance$dup == F,c("SK_ID_CURR","SK_ID_PREV","NAME_CONTRACT_STATUS")]
pc_unq1        = dcast(pc_unq,SK_ID_CURR~NAME_CONTRACT_STATUS)

colnames(pc_unq1)[2:ncol(pc_unq1)]=c(paste0("pc_num_",colnames(pc_unq1)[2:ncol(pc_unq1)]))

#Summarize pos loan history and installment details by customer
pc_dets = 
  pc_balance %>%
  filter(NAME_CONTRACT_STATUS=="Active") %>%
  group_by(SK_ID_CURR) %>%
  summarise(
    pc_mos_since_start      = min(MONTHS_BALANCE),
    pc_mos_since_active     = max(MONTHS_BALANCE),
    pc_mos_past_due         = sum(SK_DPD > 0),
    pc_avg_num_instlmnts    = mean(CNT_INSTALMENT)
  )

#Flag customers with overlapping POS loans, and calculate the amount of overlap time
pc_numact1              = data.frame(with(pc_balance[pc_balance$NAME_CONTRACT_STATUS=="Active",],
                                          table(SK_ID_CURR,MONTHS_BALANCE)))
pc_numact                = pc_numact1[pc_numact1$Freq>1,]
pc_numact$MONTHS_BALANCE = as.integer(as.character(pc_numact$MONTHS_BALANCE))
pc_numact$SK_ID_CURR     = factor(pc_numact$SK_ID_CURR)

pc_dets1 =
  pc_numact %>%
  group_by(SK_ID_CURR) %>%
  summarise(
    pc_overlap_start = min(MONTHS_BALANCE),
    pc_overlap_end   = max(MONTHS_BALANCE),
    pc_num_overlaps  = max(Freq)
  )

pc_unq1 = merge(pc_unq1,pc_dets,by="SK_ID_CURR",all.x=T)
pc_unq1 = merge(pc_unq1,pc_dets1,by="SK_ID_CURR",all.x=T)

#View(pc_unq1)
rm(pc_balance, pc_unq, pc_dets, pc_dets1, pc_numact, pc_numact1)
gc()



###############Combine credit card and POS info by customer to create history file#############
cust_hist = merge(pc_unq1,cc_unq1,by="SK_ID_CURR",all.x=T,all.y=T)
summary(cust_hist)

#Fields to convert from NA to zero
tozero = c(
  "pc_num_Active",                "pc_num_Amortized debt",
  "pc_num_Approved",              "pc_num_Canceled",
  "pc_num_Completed",             "pc_num_Demand",
  "pc_num_Returned to the store", "pc_num_Signed",
  "pc_avg_num_instlmnts",         "pc_num_overlaps",
  "cc_num_Active",                "cc_num_Completed",
  "cc_num_Demand",                "cc_num_Signed",
  "cc_avg_num_drawings",          "cc_avg_num_atm_drawings")

#Convert select NA fields to zero
for(i in 1:length(tozero)){
  cust_hist[is.na(cust_hist[,tozero[i]]),tozero[i]] = 0
}
  
summary(cust_hist)

#Additional flags
cust_hist$cc_cust_flag = cust_hist$cc_num_Active > 0 | cust_hist$cc_num_Completed > 0
cust_hist$pc_cust_flag = cust_hist$pc_num_Active > 0 | cust_hist$pc_num_Completed > 0
cust_hist$active_flag  = cust_hist$cc_num_Active > 0 | cust_hist$pc_num_Active > 0
cust_hist$dormant_flag = cust_hist$cc_num_Active == 0 & cust_hist$pc_num_Active == 0 & (cust_hist$cc_num_Completed > 0 | cust_hist$pc_num_Completed > 0)

#Save final file
saveRDS(cust_hist,"cust_hist.Rda")

rm(pc_unq1, cc_unq1)
gc()
