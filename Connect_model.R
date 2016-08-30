library(data.table)

### PARAMETERS ###

# this needs to be changed in case of new user
path<-"/Users/AlexAkimenko/Documents/работа/Citi/OSM_model/" # current working directory


### LOAD DATA ####

#db_all<-db_all[order(db_all$EventDateTime),]
#db_all<-within(db_all, {
#  n_attempt_s_phone <- ave(count, phone_type, act_date, account_number, FUN = cumsum)
#})
#db_all<-within(db_all, {
#  time_last_att <- round(ave(as.numeric(db_all$EventDateTime), account_number, act_date, FUN = smart_diff)/60)
#})
#db_all<-within(db_all, {
#  time_last_att_s_phone <- round(ave(as.numeric(db_all$EventDateTime), account_number, phone_type, act_date, FUN = smart_diff)/60)
#})
#saveRDS(db_all,"/Users/AlexAkimenko/Documents/работа/OSM model/MR.rds")

db_all<-readRDS(paste0(path,"/1_data/db_all.rds"))
MR<-as.data.table(db_all[db_all$workgroup=="MR" & db_all$account_number!="" & as.Date(db_all$EventDateTime)>as.Date("2016/2/11") & as.Date(db_all$EventDateTime)!=as.Date("2016/2/23"),])

### FUNCTIONS ####

smart_diff<-function(x){
  y<-append(0,diff(x))
  return(y)
}


### CONNECT MODEL ####

MR_pred<-MR[MR$DialModeDesc=="Predictive",]

X<-data.frame(n_attempt=MR_pred$n_attempt,
              #n_attempt_s_phone=MR_pred$n_attempt_s_phone,
              time_last_att=MR_pred$time_last_att,
              #time_last_att_s_phone=MR_pred$time_last_att_s_phone,
              hour=as.factor(as.integer(MR_pred$hour)),
              phone_type=as.factor(MR_pred$phone_type),
              weekday=as.factor(MR_pred$weekday),
              ListName=as.factor(MR_pred$ListName))   

y<-MR_pred$OutboundConnected==1
set.seed(1)
sample_flag<-sample(1:nrow(X),nrow(X)*0.7)

X_train <- X[sample_flag,]
#X_test<-X[-sample_flag,]
y_train<-y[sample_flag]
#y_test<-y[-sample_flag]

mr_connect_glm<-glm(y_train~.,X_train,family=binomial)
summary(mr_connect_glm)
saveRDS(mr_connect_glm,paste0(path,"/2_models/mr_connect_glm.rds"))

y_pred<-predict(mr_connect_glm,X,type = "response")
#y_train_pred<-predict(mr_connect_glm,X_train,type = "response")

MR$connect_pred[MR$DialModeDesc=="Predictive"]<-y_pred
MR$connect_pred[is.na(MR$connect_pred)]<-1

saveRDS(MR,paste0(path,"/1_data/MR.rds"))

MR_ag<-MR[,.(connect_rate_act=mean(OutboundConnected),
             connect_rate_pred=mean(connect_pred),
             connect_int_act=sum(OutboundConnected)/length(unique(account_number)),
             connect_int_pred=(mean(connect_pred)*.N)/length(unique(account_number))),by=.(act_date)]

MR_ag$MAPE_connect_rate<-abs(MR_ag$connect_rate_act-MR_ag$connect_rate_pred)/MR_ag$connect_rate_act
MR_ag$MAPE_connect_int<-abs(MR_ag$connect_int_act-MR_ag$connect_int_pred)/MR_ag$connect_int_act

MAPE<-mean(MR_ag$MAPE_connect_int)
MAPE
# 0.04818771

cor(MR_ag$connect_int_act,MR_ag$connect_int_pred)^2
# 0.9957624

plot(as.POSIXct(strptime(MR_ag$act_date, "%Y-%m-%d")), MR_ag$connect_int_act)
lines(as.POSIXct(strptime(MR_ag$act_date, "%Y-%m-%d")), MR_ag$connect_int_pred)
