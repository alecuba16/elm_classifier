tracking_results_df<-function(table_cast_results='CAST_RESULTS',date_time_name='date_time',type_name='pfailure',anticipation=28,threshold=0.5,db_config=NULL){
    iam=match.call()[[1]]
    
    #Dependencia basica
    if(!exists("dependencyLoader")){
        if(!file.exists('functions_common/dependencyLoader.R')) 
            return(list(error=TRUE,data=NULL,
                        msg=paste0("\n",iam,": Missing dependency function: functions_common/dependencyLoader.R")));
        source('functions_common/dependencyLoader.R')
    }
    
    # Sources
    libraries<-c('plyr')
    sources<-paste0("functions_common/", 
                    c('db_query.R',
                      'close_protocol.R',
                      'update_table_report.R'))
    dep<-dependencyLoader(c(libraries,sources))
    
    if(dep$error) stop(iam,":on call dependencyLoader\n",dep$msg)
    rm(dep)
    
    #Download data
    query<-paste0("SELECT * from CAST_RESULTS where horizon>0 and type='",type_name,"'")
    rs<-db_query(query=query,db_config=db_config)
    if(rs$error) stop(iam,":on call db_query\n",rs$msg)
    results<-rs$data
    #Datetime to posix
    results$date_time<-as.POSIXct(results$date_time,tz = 'UTC',origin='1970-01-01')
    #Generate virtual horitzon date_time
    results$virtualdt<-results$date_time+as.difftime(results$horizon, units="days")
    
    #Get the lds ids
    lds_ids<-unique(results$ld_id)
    
    #id dict alms
    query<-paste0('SELECT ld.ld_id as ld_id,nom_taula_events as events_table_name from SC_DIC_EVT_ALMS dic INNER JOIN SC_LOGICALDEVICE ld ON dic.id_dicalm=ld.id_dicalm where ld_id IN(',paste0(lds_ids,collapse = ','),')')
    rs<-db_query(query=query,db_config=db_config)
    if(rs$error) stop(iam,":on call db_query\n",rs$msg)
    dic_alarms<-rs$data
    
    df_result<-data.frame(
        ld_id=as.numeric(),
        fault=as.character(),
        id_walm=as.numeric(),
        alarm_date=as.POSIXct(as.numeric(),origin='1970-01-01',tz='UTC'),
        pred_date_time=as.POSIXct(as.numeric(),origin='1970-01-01',tz='UTC'),
        pred_horizon=as.numeric(),
        prob_fault=as.numeric())
    #for each machine
    for(ld in lds_ids){
        current_machine<-results[results$ld_id==ld,]
        alarm_table<-dic_alarms$events_table_name[dic_alarms$ld_id==ld]
        faults<-unique(current_machine$fault)
        #for each fault
        for(fault in faults){
            current_machine_fault<-current_machine[current_machine$fault==fault,]
            mindt<-min(current_machine_fault$date_time,na.rm = T)
            maxdt<-max(current_machine_fault$date_time,na.rm = T)+anticipation
            #Download alarms events for this ld_id
            alarm_codes<-current_machine_fault$array_id_walm[1]
            query<-paste0('SELECT id_walm,ld_id,date_time from ',alarm_table,' where ld_id=',ld,' and id_walm IN(',alarm_codes,') and date_time_end>FROM_UNIXTIME(',as.numeric(mindt),') and date_time<FROM_UNIXTIME(',as.numeric(maxdt),')')
            rs<-db_query(query=query,db_config=db_config)
            if(rs$error) stop(iam,":on call db_query\n",rs$msg)
            alarms<-rs$data
            if(nrow(alarms)>0){
                #Datetime to posix
                alarms$date_time<-as.POSIXct(as.Date(alarms$date_time,tz = 'UTC',origin='1970-01-01' ),tz = 'UTC',origin='1970-01-01')
                for(i in 1:nrow(alarms)){
                    current_alarm<-alarms[i,]
                    #pre_alarm_dates<-as.POSIXct(unlist(lapply(current_alarm$date_time,function(dt) seq.POSIXt(dt-as.difftime(anticipation-1, units="days"),by='day',dt))),origin='1970-01-01',tz = "UTC")
                    #pre_df<-data.frame(virtualdt=pre_alarm_dates,id_walm=rep(current_alarm$id_walm,length(pre_alarm_dates)))
                    intersect<-(current_machine_fault$virtualdt %in% current_alarm$date_time)
                    #if(any(intersect)&&(current_machine_fault$prob_fault[intersect]>threshold)){
                    if(any(intersect)){
                        pred_date_time<-unique(current_machine_fault$date_time[intersect])
                        predictions<-current_machine_fault[current_machine_fault$date_time %in% pred_date_time,]
                        if(nrow(predictions)>0){
                            for(j in 1:nrow(predictions)){
                                tmp_df<-data.frame(ld_id=ld,
                                                   fault=fault,
                                                   id_walm=current_alarm$id_walm,
                                                   alarm_date=current_alarm$date_time,
                                                   pred_date_time=predictions$date_time[j],
                                                   pred_horizon=predictions$horizon[j],
                                                   prob_fault=predictions$prob_fault[j],
                                                   stringsAsFactors = F)
                                df_result<-rbind(df_result,tmp_df)
                            }
                        }
                    }
                }
            }
        }
    }
    return(list(error=FALSE,data=df_result,msg='ok'))
}