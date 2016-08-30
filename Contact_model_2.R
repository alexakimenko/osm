install.packages("data.table")
library(data.table)
db_all<-readRDS("/Users/AlexAkimenko/Documents/работа/Citi/OSM_model/1_data/MR.rds")
MR<-db_all[db_all$workgroup=="MR"  & db_all$account_number!="" & as.Date(db_all$EventDateTime)>as.Date("2016/2/11"),]

MR_ag<-data.table()

for (att in 1:max(MR$n_attempt)){
  dt<-as.data.table(MR[MR$n_attempt<=att,])
  dt<-dt[,.(n_accts=length(unique(account_number)),
            attempt_int=.N/length(unique(account_number)),
            connect_int=sum(OutboundConnected)/length(unique(account_number)),
            contact_int=sum(RPC)/length(unique(account_number))),
         by=.(act_date,weekday,ListName,DialModeDesc,phone_type)]
  dt$n_attempt=att
  MR_ag<-rbind(MR_ag,dt)
}


X<-data.table(attempt_int=MR_ag$attempt_int,
              connect_int=MR_ag$connect_int,
              phone_type=as.factor(MR_ag$phone_type),
              mode=as.factor(MR_ag$DialModeDesc),
              weekday=as.factor(MR_ag$weekday),
              ListName=as.factor(MR_ag$ListName))   

y<-MR_ag$contact_int
set.seed(1)
sample_flag<-sample(1:nrow(X),nrow(X)*0.7)

X_train <- X[sample_flag,]
X_test<-X[-sample_flag,]
y_train<-y[sample_flag]
y_test<-y[-sample_flag]

mr_contact_lm<-lm(y_train~.,X_train)
summary(mr_contact_lm)
#saveRDS(mr_contact_glm,"/Users/AlexAkimenko/Documents/работа/Citi/OSM_model/2_models/mr_contact_glm.rds")

y_test_pred<-predict(mr_contact_lm,X_test,type = "response")
y_train_pred<-predict(mr_contact_lm,X_train,type = "response")

MR_ag[,.(act_date,
         contact_int=sum(n_accts*contact_int)/sum(n_accts)),
      by=act_date]

df_date<-data.frame(act_date=as.POSIXct(strptime(act_date, "%Y-%m-%d")),actual=as.numeric(actual),pred=as.numeric(pred))
MAPE<-mean(abs(df_date$actual-df_date$pred)/df_date$actual)
MAPE
# 0.134977 hour=as.factor+ n_attempt+n_attempt_s_phone+time_last_att+time_last_att_s_phone+ phone_type+mode+ListName+weekday
cor(df_date$actual,df_date$pred)^2
# 0.91 determination

t<-df_date[act_date!=as.Date("2016/2/23"),]
MAPE<-mean(abs(t$actual-t$pred)/t$actual)
MAPE
#0.09655357 
cor(t$actual,t$pred)^2
#0.9445913

plot(df_date$act_date, df_date$actual)
lines(df_date$act_date, df_date$pred)
