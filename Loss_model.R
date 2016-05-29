
# input data
hours_list<-as.factor(09:20) # временные интервалы, когда совершаются звонки
promises<-data.frame(TP_rate=0.5, KP_rate=0.5) #исторические данные о количестве TP и KP рейтах
cost_connect<-100 #from intencity cap plan: cost for team/total connects
save_contact<-1000 # new Save model to be developed

# input models and new variables
mr_connect_glm<-readRDS("/Users/AlexAkimenko/Documents/работа/OSM model/mr_connect_glm.rds")
mr_contact_glm<-readRDS("/Users/AlexAkimenko/Documents/работа/OSM model/mr_contact_glm.rds")
OSM<-data.frame() #здесь будет лежать последовательности попыток и их EBIT
OSMi<-data.frame()
attempt=1

for (hour in hours_list){
  for (phone_type in c("m", "h", "b")){
    for (ListName in c("RU_C_X_M_NC_RSK","RU_C_X_M_CONT")){
      for (mode in c("Predictive", "Preview   ")){
        X_connect<-data.frame(n_attempt=attempt,n_attempt_s_phone=0,hour=hour,
                              time_last_att=60, phone_type=phone_type,
                              ListName=ListName,mode=mode )
        if (mode=="Predictive"){
          X_connect$connect_rate<-predict(mr_connect_glm, X_connect,type = "response")
        } else {
          X_connect$connect_rate<-1
        }
        X_connect$cost<-cost_connect*X_connect$connect_rate
        X_connect$hour<-as.numeric(levels(hour))[hour]
        X_connect$contact_rate<-predict(mr_contact_glm, X_connect,type = "response")
        X_connect$save<-save_contact*X_connect$contact_rate*X_connect$connect_rate        
        OSMi<-rbind(OSMi,X_connect)
      }

    }
  }
  attempt=attempt+1
}



X_connect<-data.frame(n_attempt=1,n_attempt_s_phone=0,hour=hours_list[1],
                      time_last_att=60, phone_type="m",
                      ListName="RU_C_X_M_NC_RSK",mode="Predictive" )

X_connect$hour<-as.numeric(levels(hours_list[1]))[hours_list[1]]

For hour, attempt in [hours_list,range(1:length(hours_list))]:
  Att=[] #задаем лист значений (EBIT, connect rate,…) для i-й попытки
For phone in ["m", "h", "b", "o"]:
  For mode in [«auto»,"manual"]:
  X_connect=[hour, attempt, phone, mode]
connect_rate=predict(connect_glm, X_connect)
contact_rate=predict(contact_glm, X_connect)
atttempt_int=attempt_intensity(
  X_soc_flow=[atttempt_int,contact_int,connect_int, tp_int, kp_int]
  soc_flow=predict(SOC_flow_lm, X_soc_flow)
  Save=(soc_flow-soc_flow_0) * avg_balance * SOC_flowsi+1 *… * SOC_flowsi+n
  Loss=Connects*AHT*Availability*Occupancy*(1+%inbound prod time)*Cost_FTE
  EBIT=Save-Loss
  I=[EBIT,Save, Loss,hour,phone,mode,attempt,connect_rate,contact_rate,attempt_intensity, SOC_flow]
  Att.append(I)
  Att=argmax(ebit,Att)
  OSM.append(Att)
  
