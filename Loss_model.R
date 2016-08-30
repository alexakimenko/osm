
# input data
hours_list<-as.factor(9:20) # временные интервалы, когда совершаются звонки
promises<-data.frame(TP_rate=0.5, KP_rate=0.5) #исторические данные о количестве TP и KP рейтах
cost_connect_pred<-100 #from intencity cap plan: cost for team/total connects 
cost_connect_prev<-cost_connect_pred
#cost_connect_prev<-cost_connect_pred*1.23
save_contact<-1000 # new Save model to be developed

rpc_int<-function(rpc_rate){
  rpc_rate_lag<-c(0,rpc_rate)
  accounts_left<-cumprod(1-rpc_rate_lag)
  rpc_int<-sum(rpc_rate*accounts_left)
  return(rpc_int)
}

connect_int<-function(connect_rate,rpc_rate){
  rpc_rate_lag<-c(0,rpc_rate)
  accounts_left<-cumprod(1-rpc_rate_lag)
  connect_int<-sum(connect_rate*accounts_left)
  return(connect_int)
}

# input models and new variables
mr_connect_glm<-readRDS("/Users/AlexAkimenko/Documents/работа/Citi/OSM_model/2_models/mr_connect_glm.rds")
mr_contact_glm<-readRDS("/Users/AlexAkimenko/Documents/работа/Citi/OSM_model/2_models/mr_contact_glm.rds")
mr_rpc_glm<-readRDS("/Users/AlexAkimenko/Documents/работа/Citi/OSM_model/2_models/mr_rpc_glm.rds")

OSM<-data.frame() #здесь будет лежать последовательности попыток и их EBIT

# ListName<-"RU_C_X_M_NC_RSK"
# mode <- "Predictive"
# hour<- hours_list[2]
# phone_type <-"b"
# attempt=2
for (ListName in c("RU_C_X_M_NC_RSK","RU_C_X_M_CONT")){
  for (mode in c("Predictive", "Preview   ")){
    OSM_i<-data.frame()
    attempt=1
    for (hour in hours_list){
      t <-data.frame()
      for (phone_type in c("m", "h", "b")){
        X<-data.frame(n_attempt=attempt,
                      n_attempt_s_phone=sum(OSM_i$phone_type==phone_type)+1,hour=hour,
                      time_last_att=ifelse(attempt==1,0,60), phone_type=phone_type,
                      ListName=ListName,mode=mode,weekday=as.factor(3))
        if (mode=="Predictive"){
          X$connect_pred<-predict(mr_connect_glm, X,type = "response")
          X$cost<-cost_connect_pred*X$connect_pred
        } else {
          X$connect_pred<-1
          X$cost<-cost_connect_prev*X$connect_pred
        }
        X$contact_pred<-predict(mr_contact_glm, X,type = "response")
        X$hour<-as.integer(as.character(X$hour))
        X$rpc_pred<-predict(mr_rpc_glm, X,type = "response")
        X$attempt_int<-connect_int(rep(1,attempt),OSM_i$rpc_pred)
        X$connect_int<-connect_int(c(OSM_i$connect_pred,X$connect_pred),OSM_i$rpc_pred)
        X$contact_int<-connect_int(c(OSM_i$contact_pred,X$contact_pred),OSM_i$rpc_pred)
        X$rpc_int<-rpc_int(c(OSM_i$rpc_pred,X$rpc_pred))
        X$save<-save_contact*X$rpc_pred
        X$margin<-X$save-X$cost
        t<-rbind(t,X)
      }
    OSM_i<-rbind(OSM_i,t[which.max(t$margin),])
    attempt=attempt+1
    }
    OSM<-rbind(OSM,OSM_i)
  }
}

OSM
pred<-OSM[OSM$mode=="Predictive",]
prev<-OSM[OSM$mode=="Preview   ",]

pred_cont<-pred[pred$ListName=="RU_C_X_M_CONT",]
prev_cont<-prev[prev$ListName=="RU_C_X_M_CONT",]

pred_nc<-pred[pred$ListName=="RU_C_X_M_NC_RSK",]
prev_nc<-prev[prev$ListName=="RU_C_X_M_NC_RSK",]
