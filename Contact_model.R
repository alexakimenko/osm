db_all<-readRDS("/Users/AlexAkimenko/Documents/работа/OSM model/MR.rds")
MR_connect<-db_all[db_all$workgroup=="MR"  & db_all$OutboundConnected==1 & db_all$account_number!="" & as.Date(db_all$EventDateTime)>as.Date("2016/2/11"),]


X<-data.frame(n_attempt=MR_connect$n_attempt,
              n_attempt_s_phone=MR_connect$n_attempt_s_phone,
              #time_last_att=MR_connect$time_last_att,
              #time_last_att_s_phone=MR_connect$time_last_att_s_phone,
              hour=as.integer(MR_connect$hour),
              phone_type=as.factor(MR_connect$phone_type),
              mode=as.factor(MR_connect$DialModeDesc),
              ListName=as.factor(MR_connect$ListName),
              #weekday=as.factor(MR_connect$weekday),
              act_date=MR_connect$act_date)   

y<-MR_connect$RPC==1
set.seed(1)
sample_flag<-sample(1:nrow(X),nrow(X)*0.7)

X_train <- X[sample_flag,]
X_test<-X[-sample_flag,]
y_train<-y[sample_flag]
y_test<-y[-sample_flag]


mr_contact_glm<-glm(y_train~.,X_train[,-7],family=binomial)
summary(mr_contact_glm)
saveRDS(mr_contact_glm,"/Users/AlexAkimenko/Documents/работа/OSM model/mr_contact_glm.rds")

y_test_pred<-predict(mr_contact_glm,X_test[,-7],type = "response")


actual=""
pred=""
act_date=""
i=1
for (day in unique(X_test$act_date)){
  actual[i]<-sum(y_test[X_test$act_date==day])/length(y_test[X_test$act_date==day])
  pred[i]<-mean(y_test_pred[X_test$act_date==day])
  act_date[i]<-day
  i=i+1
}


df_date<-data.frame(act_date=as.POSIXct(strptime(act_date, "%Y-%m-%d")),actual=as.numeric(actual),pred=as.numeric(pred))
MAPE<-mean(abs(df_date$actual-df_date$pred)/df_date$actual)
MAPE
# 0.134977 hour=as.factor+ n_attempt+n_attempt_s_phone+time_last_att+time_last_att_s_phone+ phone_type+mode+ListName+weekday

plot(df_date$act_date, df_date$actual)
lines(df_date$act_date, df_date$pred)
