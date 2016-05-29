db_all<-readRDS("/Users/AlexAkimenko/Documents/работа/OSM model/MR.rds")
db_all<-db_all[order(db_all$EventDateTime),]

smart_diff<-function(x){
  y<-append(0,diff(x))
  return(y)
}

db_all<-within(db_all, {
  n_attempt_s_phone <- ave(count, phone_type, act_date, account_number, FUN = cumsum)
})

db_all<-within(db_all, {
  time_last_att <- round(ave(as.numeric(db_all$EventDateTime), account_number, act_date, FUN = smart_diff)/60)
})
db_all<-within(db_all, {
  time_last_att_s_phone <- round(ave(as.numeric(db_all$EventDateTime), account_number, phone_type, act_date, FUN = smart_diff)/60)
})

saveRDS(db_all,"/Users/AlexAkimenko/Documents/работа/OSM model/MR.rds")


MR_pred<-db_all[db_all$workgroup=="MR"  & db_all$DialModeDesc=="Predictive" & db_all$account_number!="" & as.Date(db_all$EventDateTime)>as.Date("2016/2/11"),]

X<-data.frame(n_attempt=MR_pred$n_attempt,
              n_attempt_s_phone=MR_pred$n_attempt_s_phone,
              time_last_att=MR_pred$time_last_att,
              #time_last_att_s_phone=MR_pred$time_last_att_s_phone,
              hour=as.factor(as.integer(MR_pred$hour)),
              phone_type=as.factor(MR_pred$phone_type),
              #mode=as.factor(MR_pred$DialModeDesc),
              ListName=as.factor(MR_pred$ListName),
              #weekday=as.factor(MR_pred$weekday),
              act_date=MR_pred$act_date)   

y<-MR_pred$OutboundConnected==1
set.seed(1)
sample_flag<-sample(1:nrow(X),nrow(X)*0.7)

X_train <- X[sample_flag,]
X_test<-X[-sample_flag,]
y_train<-y[sample_flag]
y_test<-y[-sample_flag]


mr_connect_glm<-glm(y_train~.,X_train[,-7],family=binomial)
summary(mr_connect_glm)
saveRDS(mr_connect_glm,"/Users/AlexAkimenko/Documents/работа/OSM model/mr_connect_glm.rds")

y_test_pred<-predict(mr_connect_glm,X_test[,-7],type = "response")

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
# 0.1528123 hour=as.factor+ n_attempt+phone_type+mode+ListName+weekday
# 0.08180364 as.Date(db_all$EventDateTime)>as.Date("2016/2/11")
plot(df_date$act_date, df_date$actual)
lines(df_date$act_date, df_date$pred)

##### investigate

actual=""
pred=""
n_attempt=""
for (i in 1:27){
  actual[i]<-sum(y_test[X_test$n_attempt==i])/length(y_test[X_test$n_attempt==i])
  pred[i]<-mean(y_test_pred[X_test$n_attempt==i])
  n_attempt[i]<-i
}

df<-data.frame(n_attempt=as.numeric(n_attempt),actual=as.numeric(actual),pred=as.numeric(pred))

plot(df$n_attempt, df$actual)
lines(df$pred)
