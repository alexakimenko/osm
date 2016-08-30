library(data.table)

### PARAMETERS ###

# this needs to be changed in case of new user
path<-"/Users/AlexAkimenko/Documents/работа/Citi/OSM_model/" # current working directory

### LOAD DATA ####

MR<-readRDS(paste0(path,"/1_data/MR.rds"))


### CONTACT (WPC+RPC) MODEL ####

X<-data.frame(#n_attempt=MR$n_attempt,
              n_attempt_s_phone=MR$n_attempt_s_phone,
              connect_pred=MR$connect_pred,
              #time_last_att=MR$time_last_att,
              #time_last_att_s_phone=MR$time_last_att_s_phone,
              #hour=as.integer(MR$hour),
              phone_type=as.factor(MR$phone_type),
              mode=as.factor(MR$DialModeDesc),
              weekday=as.factor(MR$weekday),
              ListName=as.factor(MR$ListName))   

y<-MR$Contact==1
set.seed(1)
sample_flag<-sample(1:nrow(X),nrow(X)*0.7)

X_train <- X[sample_flag,]
#X_test<-X[-sample_flag,]
y_train<-y[sample_flag]
#y_test<-y[-sample_flag]

#x<-cbind(X,y)
#cor(x)
#corrgram(x, order=TRUE, lower.panel=panel.shade,
#  upper.panel=panel.pie, text.panel=panel.txt,
#  main="Car Milage Data in PC2/PC1 Order")

mr_contact_glm<-glm(y_train~.,X_train,family=binomial)
summary(mr_contact_glm)
saveRDS(mr_contact_glm,paste0(path,"/2_models/mr_contact_glm.rds"))
# mr_contact_glm<-readRDS("/Users/AlexAkimenko/Documents/работа/Citi/OSM_model/2_models/mr_contact_glm.rds")

y_pred<-predict(mr_contact_glm,X,type = "response")
#y_train_pred<-predict(mr_contact_glm,X_train,type = "response")

MR$contact_pred<-y_pred

MR_ag<-MR[,.(contact_rate_act=mean(Contact),
             contact_rate_pred=mean(contact_pred),
             contact_int_act=sum(Contact)/length(unique(account_number)),
             contact_int_pred=(mean(contact_pred)*.N)/length(unique(account_number))),by=.(act_date)]

MR_ag$MAPE_contact_rate<-abs(MR_ag$contact_rate_act-MR_ag$contact_rate_pred)/MR_ag$contact_rate_act
MR_ag$MAPE_contact_int<-abs(MR_ag$contact_int_act-MR_ag$contact_int_pred)/MR_ag$contact_int_act

MAPE<-mean(MR_ag$MAPE_contact_int)
MAPE
# 0.08465604

cor(MR_ag$contact_int_act,MR_ag$contact_int_pred)^2
# 0.9675689

plot(as.POSIXct(strptime(MR_ag$act_date, "%Y-%m-%d")), MR_ag$contact_int_act)
lines(as.POSIXct(strptime(MR_ag$act_date, "%Y-%m-%d")), MR_ag$contact_int_pred)


### RPC MODEL ####

X<-data.frame(#n_attempt=MR$n_attempt,
              n_attempt_s_phone=MR$n_attempt_s_phone,
              connect_pred=MR$connect_pred,
              contact_pred=MR$contact_pred,
              #time_last_att=MR$time_last_att,
              #time_last_att_s_phone=MR$time_last_att_s_phone,
              hour=as.integer(MR$hour),
              phone_type=as.factor(MR$phone_type),
              mode=as.factor(MR$DialModeDesc),
              weekday=as.factor(MR$weekday),
              ListName=as.factor(MR$ListName))   

y<-MR$RPC==1
set.seed(1)
sample_flag<-sample(1:nrow(X),nrow(X)*0.7)

X_train <- X[sample_flag,]
#X_test<-X[-sample_flag,]
y_train<-y[sample_flag]
#y_test<-y[-sample_flag]


#x<-cbind(X,y)
#cor(x)
#corrgram(x, order=TRUE, lower.panel=panel.shade,
#  upper.panel=panel.pie, text.panel=panel.txt,
#  main="Car Milage Data in PC2/PC1 Order")

mr_rpc_glm<-glm(y_train~.,X_train,family=binomial)
summary(mr_rpc_glm)
saveRDS(mr_rpc_glm,paste0(path,"/2_models/mr_rpc_glm.rds"))
# mr_rpc_glm<-readRDS("/Users/AlexAkimenko/Documents/работа/Citi/OSM_model/2_models/mr_rpc_glm.rds")

y_pred<-predict(mr_rpc_glm,X,type = "response")


MR$rpc_pred<-y_pred

MR_ag<-MR[,.(rpc_rate_act=mean(RPC),
             rpc_rate_pred=mean(rpc_pred),
             rpc_int_act=sum(RPC)/length(unique(account_number)),
             rpc_int_pred=(mean(rpc_pred)*.N)/length(unique(account_number))),by=.(act_date)]

MR_ag$error_rpc_rate<-(MR_ag$rpc_rate_act-MR_ag$rpc_rate_pred)/MR_ag$rpc_rate_act
MR_ag$error_rpc_int<-(MR_ag$rpc_int_act-MR_ag$rpc_int_pred)/MR_ag$rpc_int_act
summary(MR_ag)

MAPE<-mean(abs(MR_ag$error_rpc_int))
MAPE
# 0.09087126

R2<-cor(MR_ag$rpc_int_act,MR_ag$rpc_int_pred)^2
R2
# 0.9429951

plot(as.POSIXct(strptime(MR_ag$act_date, "%Y-%m-%d")), MR_ag$rpc_int_act, 
     xlab= "Date",
     ylab = "RPC intencity", 
     main = paste0("Actual RPC intencity vs predicted - MAPE=",round(MAPE*100,1),"%, R^2=",round(R2*100,1),"%"))
lines(as.POSIXct(strptime(MR_ag$act_date, "%Y-%m-%d")), MR_ag$rpc_int_pred)



### SUCCESS MODEL ####

X<-data.frame(#n_attempt=MR$n_attempt,
  n_attempt_s_phone=MR$n_attempt_s_phone,
  connect_pred=MR$connect_pred,
  contact_pred=MR$contact_pred,
  rpc_pred=MR$rpc_pred,
  #time_last_att=MR$time_last_att,
  #time_last_att_s_phone=MR$time_last_att_s_phone,
  hour=as.integer(MR$hour),
  phone_type=as.factor(MR$phone_type),
  mode=as.factor(MR$DialModeDesc),
  weekday=as.factor(MR$weekday),
  ListName=as.factor(MR$ListName))   

y<-MR$Success==1
set.seed(1)
sample_flag<-sample(1:nrow(X),nrow(X)*0.7)

X_train <- X[sample_flag,]
#X_test<-X[-sample_flag,]
y_train<-y[sample_flag]
#y_test<-y[-sample_flag]


#x<-cbind(X,y)
#cor(x)
#corrgram(x, order=TRUE, lower.panel=panel.shade,
#  upper.panel=panel.pie, text.panel=panel.txt,
#  main="Car Milage Data in PC2/PC1 Order")

mr_success_glm<-glm(y_train~.,X_train,family=binomial)
summary(mr_success_glm)
saveRDS(mr_success_glm,paste0(path,"/2_models/mr_success_glm.rds"))
# mr_success_glm<-readRDS("/Users/AlexAkimenko/Documents/работа/Citi/OSM_model/2_models/mr_success_glm.rds")

y_pred<-predict(mr_success_glm,X,type = "response")


MR$success_pred<-y_pred

MR_ag<-MR[,.(success_rate_act=mean(Success),
             success_rate_pred=mean(success_pred),
             success_int_act=sum(Success)/length(unique(account_number)),
             success_int_pred=(mean(success_pred)*.N)/length(unique(account_number))),by=.(act_date)]

MR_ag$error_success_rate<-(MR_ag$success_rate_act-MR_ag$success_rate_pred)/MR_ag$success_rate_act
MR_ag$error_success_int<-(MR_ag$success_int_act-MR_ag$success_int_pred)/MR_ag$success_int_act
summary(MR_ag)

MAPE<-mean(abs(MR_ag$error_success_int))
MAPE
# 0.09087126

R2<-cor(MR_ag$success_int_act,MR_ag$success_int_pred)^2
R2
# 0.9429951

plot(as.POSIXct(strptime(MR_ag$act_date, "%Y-%m-%d")), MR_ag$success_int_act, 
     xlab= "Date",
     ylab = "success intencity", 
     main = paste0("Actual success intencity vs predicted - MAPE=",round(MAPE*100,1),"%, R^2=",round(R2*100,1),"%"))
lines(as.POSIXct(strptime(MR_ag$act_date, "%Y-%m-%d")), MR_ag$success_int_pred)