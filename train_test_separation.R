comb <- function(results, x) {
  results[x$rmin:x$rmax,1:x$timesteps,] <- x$result
  results
}

#Balance -> number of pre_alarm false in proportion of true in percentage.
train_test_separation<-function(train_machines=NULL,wtdata=NULL,target_name=NULL,use_alarms=F,use_ots=F,use_health=F,anticipation=56,lstm_format=F,seconds_to_aggregate=86400,train_seconds_ot_marging=86400,test_seconds_ot_marging=NULL,tmpfolder='tmp',balance=NULL,select_good_same_bad_interval=F,logfile=NULL,normalize_exclude_columns='date_time,ld_id,ot,ot_block_code,alarm,alarm_block_code,alarm_all,alarm_all_block_code,pre_alarm'){
  iam=match.call()[[1]]
  set.seed(1)
  
  library(parallel)
  library(doSNOW)
  library(bigmemory)
  
  #checks
  if(use_health&&(!('health_status' %in% names(wtdata))||length(unique(wtdata$health_status,na.rm = T))==1)) return(list(error=T,data=NULL,msg='health_status column doesn\'t exists on wtdata or health_status is unique'))
  if(use_ots&&(!('ot' %in% names(wtdata))||length(unique(wtdata$ot,na.rm = T))==1)) return(list(error=T,data=NULL,msg='ot column doesn\'t exists on wtdata or ot is unique'))
  if(use_alarms&&(!('alarm' %in% names(wtdata))||length(unique(wtdata$alarm,na.rm = T))==1)) return(list(error=T,data=NULL,msg='alarm column doesn\'t exists on wtdata or alarm is unique'))
  
  #lstm pre_alarm is 0 days
  if(lstm_format){
    timesteps<-anticipation
  }
  #Set bad healthstatus days before as prealarm
  lds<-unique(wtdata$ld_id)
  wtdata$pre_alarm<-F
  wtdata$pre_alarm_0_anti<-F
  wtdata$date_time<-as.POSIXct(wtdata$date_time,tz='UTC',origin='1970-01-01')
  for(ld in lds){
    current_turbine<-wtdata[wtdata$ld_id==ld,]
    if(use_health&&any(current_turbine$health_status=='bad',na.rm = T)){
      current_bad<-current_turbine$date_time[(current_turbine$health_status=='bad')]
      max_dt<-max(current_bad,na.rm = T)
      bad_dates<-seq.POSIXt(max_dt-as.difftime(anticipation, units="days"),by=paste0(seconds_to_aggregate,' sec'),max_dt,tz='UTC',origin='1970-01-01')
      wtdata$pre_alarm[(wtdata$ld_id==ld)&(wtdata$date_time %in% bad_dates)]<-T
    }else if((use_ots&&('ot' %in% names(current_turbine))&&any(current_turbine$ot==1,na.rm = T))||(use_alarms&&('alarm' %in% names(current_turbine))&&any(current_turbine$alarm==1,na.rm = T))){
      current_bad<-NULL
      if(use_ots) current_bad<-current_turbine$date_time[(current_turbine$ot==1)]
      if(use_alarms) current_bad<-unique(c(current_bad,current_turbine$date_time[(current_turbine$alarm==1)]))
      if(!is.null(current_bad)){
        current_bad<-as.POSIXct(current_bad,tz='UTC',origin='1970-01-01')
        wtdata$pre_alarm_0_anti[(wtdata$ld_id==ld)&(wtdata$date_time %in% current_bad)]<-T
        bad_dates<-as.POSIXct(sapply(1:length(current_bad),function(dt) seq.POSIXt(current_bad[dt]-as.difftime(anticipation, units="days"),by=paste0(seconds_to_aggregate,' sec'),current_bad[dt], tz='UTC',origin='1970-01-01')),tz = 'UTC',origin='1970-01-01')
        bad_dates<-unique(bad_dates)
        wtdata$pre_alarm[(wtdata$ld_id==ld)&(wtdata$date_time %in% bad_dates)]<-T
      }
    }
  }
  rm(list=ls()[ls() %in% c('current_turbine','max_dt','bad_dates','current_bad')]) #Free space

  #separation
  if(use_health||is.null(train_machines)||length(train_machines)<=1){
    if(use_health){
      train_machines<-unique(wtdata$ld_id[!is.null(wtdata$health_status)&!is.na(wtdata$health_status)])
    }else{ #train_machines is percentage
      #count pre_alarms per ld_id
      bad_lds<-unique(wtdata$ld_id[wtdata$pre_alarm==T])
      number_of_bad<-ceiling(train_machines*length(bad_lds)/100)
      bad_lds<-sample(bad_lds,size = number_of_bad)
      good_lds<-unique(wtdata$ld_id[(wtdata$pre_alarm==F)&!(wtdata$ld_id %in% bad_lds)])
      number_of_good<-ceiling(train_machines*length(good_lds)/100)
      good_lds<-sample(good_lds,size = number_of_good)
      train_machines<-c(good_lds,bad_lds)
    }
  }
  
  
  #Separate train-test
  if(is.null(train_machines)||!(any(train_machines %in% unique(wtdata$ld_id)))) return(list(error=T,data=NULL,msg='There is no train machines in wtdata'))
  train<-wtdata[wtdata$ld_id %in% abs(train_machines),]
  #Order
  train<-train[order(train$ld_id,train$date_time),]
  
  #Backup ot/alarm id
  if('ot_block_code' %in% names(train)){
    train_ot_block_code<-train[,c('ld_id','ot_block_code','date_time')]
  }
  
  if('alarm_block_code' %in% names(train)){
    train_alarm_block_code<-train[,c('ld_id','alarm_block_code','date_time')]
  }
  
  if('ot_all' %in% names(train)){
    train_ot_all<-train[,c('ld_id','ot_all','date_time')]
    train_ot<-train[,c('ld_id','ot','date_time')]
    train_ot$ot[train_ot$ot==1]<-T
    train_ot$ot[train_ot$ot==0]<-F
    if(!is.null(train_seconds_ot_marging)&&!is.na(train_seconds_ot_marging)&&train_seconds_ot_marging>0){
      others_ots_dt<-unique(train[((train$ot_all==1)&(train$pre_alarm==0)),c('date_time','ld_id')])
      train_ot_all_marging<-lapply(1:nrow(others_ots_dt),function(i) data.frame(ld_id=others_ots_dt$ld_id[i],date_time=seq.POSIXt(others_ots_dt$date_time[i]-as.difftime(train_seconds_ot_marging, units="secs"),by=paste0(seconds_to_aggregate,' sec'),others_ots_dt$date_time[i]+as.difftime(train_seconds_ot_marging, units="secs"),tz='UTC',origin='1970-01-01')))
      train_ot_all_marging<-do.call("rbind",train_ot_all_marging)
      train_ot_all_marging<-unique(train_ot_all_marging)
      train_ot_all_marging$date_time<-as.POSIXct(train_ot_all_marging$date_time,tz = 'UTC',origin='1970-01-01')
      train_excluded_by_marging<-NULL
      for(ld in unique(train_ot_all_marging$ld_id)) train_excluded_by_marging<-c(train_excluded_by_marging,which((train$pre_alarm==0)&(train$ld_id==ld)&(train$date_time %in% train_ot_all_marging$date_time[train_ot_all_marging$ld_id==ld])))
      if(length(train_excluded_by_marging)>0) train<-train[-train_excluded_by_marging,]
    }
    train$ot_all<-NULL
  }
  
  #If train_machines have negative sign (use only for pre_alarm==T) remove pre_alarm==F of these cases
  if(any(train_machines<0)){
    for(ld in train_machines[train_machines<0]){
      train<-train[!((train$ld_id==ld)&(train$pre_alarm==F)),]
    }
  }
  
  train_selected_rows<-1:nrow(train)
  #Balance
  if(!is.null(balance)&&!is.na(balance)&&balance<100&&balance>0){
    bad_selected_rows<-which(train$pre_alarm==T) #Preselect alarms
    bad_selected_datetime<-train$date_time[bad_selected_rows]
    max_bad<-sum(train$pre_alarm==T)
    max_good<-sum(train$pre_alarm==F)
    
    total<-(max_bad*100)/balance
    num_wanted_good<-round(total-max_bad,0)
    #Add same datetime as bad from good
    possible_good<-which(train$pre_alarm==F)
    real_balance<-length(train_selected_rows)*100/(length(possible_good)+length(train_selected_rows)) #Balance with available good.
    if(select_good_same_bad_interval)
      already_selected_good<-possible_good[(train$date_time[possible_good] %in% bad_selected_datetime)] #Selects from good the same date_time as  from bad.
    else
      already_selected_good<-as.numeric()
    possible_good<-possible_good[!(possible_good %in% already_selected_good)]
    good_selected_rows<-c(already_selected_good,sample(possible_good, size = min(length(possible_good),num_wanted_good-length(already_selected_good)), replace = F))
    train_selected_rows<-c(bad_selected_rows,good_selected_rows)
  }
  
  #Normalize
  if(!is.null(normalize_exclude_columns)){
    exclude_columns<-colnames(train) %in% c(unlist(strsplit(normalize_exclude_columns,split = ',')),'pre_alarm_0_anti')
  }else{
    exclude_columns<-rep(F,ncol(train))
  }
  train_na_columns<-colSums(is.na(train[,!exclude_columns]))>=nrow(train) #All the column is NA
  if(any(train_na_columns)){
    train_na_columns_names<-colnames(train[,!exclude_columns])[train_na_columns]
    train<-train[,!exclude_columns]
  }
  train_sdv<-apply(train[,!exclude_columns],2,sd,na.rm=T)
  train_zero_sdv_columns_names<-NULL
  if(any((train_sdv==0))){
    train_zero_sdv_columns_names<-colnames(train[,!exclude_columns])[(train_sdv==0)]
    train<-train[,!(colnames(train) %in% train_zero_sdv_columns_names)]
    train_sdv<-train_sdv[train_sdv!=0]
  }
  train_mean<-apply(train[,!exclude_columns],2,mean,na.rm=T)
  train[,!exclude_columns]<-sapply(colnames(train[,!exclude_columns]),function(c) {(train[,c] - train_mean[c])/train_sdv[c]})
  train_stat<-data.frame(name=colnames(train[,!exclude_columns]),sdv=train_sdv,mean=train_mean)
  #Add excluded columns with mean 0 and sdv 1 just in case!.
  colnames_excluded<-colnames(train)[exclude_columns]
  colnames_excluded<-colnames_excluded[order(colnames_excluded)]
  train_stat<-rbind(train_stat,data.frame(name=colnames_excluded,sdv=1,mean=0))
  rownames(train_stat)[train_stat$name %in% colnames_excluded]<-colnames_excluded
  
  if(lstm_format){#Prepare timesteps
    train_lstm<-NULL
    columns<-which(!(colnames(train) %in% c('pre_alarm','pre_alarm_0_anti','ot','ot_all','alarm','alarm_all','ot_block_code','alarm_block_code')))
    columns_names<-colnames(train)[columns]
    mb_per_thread<-500 #Worst case 500MB
    if(Sys.info()[['sysname']]=="Linux"){
      free<-as.numeric(system("awk '/MemAvailable/ {print $2}' /proc/meminfo",intern=TRUE))
    }else{#windows
      free<-as.numeric(gsub(x=paste0(system("wmic OS get FreePhysicalMemory /Value",intern = T),collapse = ''),pattern = '.*=([0-9]+)',replacement = '\\1'))
    }
    if(is.na(free)||is.nan(free)||is.infinite(free)) free<-1048576
    num_threads<-floor((free/1024)/mb_per_thread)
    num_threads<-min(num_threads,nrow(train))
    num_threads<-min(num_threads,parallel::detectCores())
    num_threads<-min(nrow(train),floor(num_threads))
    num_threads<-max(num_threads,1) #If numthreads is 0 force to be 1.
    if(is.null(logfile)||is.na(logfile)){
      cl <- parallel::makePSOCKcluster(num_threads)
    }else{
      cl <- parallel::makePSOCKcluster(num_threads,outfile=logfile)
    }
    doSNOW::registerDoSNOW(cl)
    gc(verbose=F)
    tmp<-bigmemory::as.big.matrix(train)
    mdesc <- describe(tmp)
    train_selected_rows<-train_selected_rows[order(train_selected_rows)]
    date_time_column<-which('date_time'==colnames(train))
    ld_id_column<-which('ld_id'==colnames(train))
    train_lstm <- foreach(r=seq(1,length(train_selected_rows),num_threads),.init = array(NA,dim=c(length(train_selected_rows),timesteps,length(columns)),dimnames = list(1:length(train_selected_rows),1:timesteps,columns_names)),.packages = 'bigmemory',.verbose = F, .combine = 'comb') %dopar% {
      tr<-bigmemory::attach.big.matrix(mdesc)
      row_max<-min(r+num_threads-1,length(train_selected_rows))
      tmp_array<-array(NA,dim=c(row_max-r+1,timesteps,length(columns)),dimnames = list(r:row_max,1:timesteps,columns))
      tmp_dt<-as.POSIXct(tr[,date_time_column],tz = 'UTC',origin='1970-01-01')
      for(cr in r:row_max){
        selected_ld<-tr[train_selected_rows[cr],ld_id_column]
        is_bad<-(train_selected_rows[cr] %in% bad_selected_rows)
        selected_dates<-seq.POSIXt(from = tmp_dt[train_selected_rows[cr]]-as.difftime(timesteps-1, units="days"),by=paste0(seconds_to_aggregate,' sec'),to =tmp_dt[train_selected_rows[cr]],tz='UTC',origin='1970-01-01')
        for(j in 1:length(selected_dates)){
          selected_row<-(selected_dates[j]==tmp_dt)&(tr[,ld_id_column]==selected_ld)
          if((any(selected_row)&&is_bad)||(any(selected_row)&&(!is_bad&&!(which(selected_row) %in% bad_selected_rows)))){#Exists current date
            tmp_array[(cr-r+1),j,]<-tr[selected_row,columns]
          }
        }
      }
      return(list(rmin=r,rmax=row_max,result=tmp_array,timesteps=timesteps))
    }
    stopCluster(cl)
    gc(verbose = F)
    train_lstm_y<-train$pre_alarm_0_anti[train_selected_rows]
  }
  
  if('alarm_all' %in% names(train)){
    train_alarm_all<-train[,c('alarm_all','ld_id','date_time')]
    train$alarm_all<-NULL
  }
  
  if('health_status' %in% names(train)){
    train_health_status<-train[,c('health_status','ld_id','date_time')]
    train$health_status<-NULL
  }
  
  #test
  test<-wtdata[!(wtdata$ld_id %in% abs(train_machines)),]
  test<-test[order(test$ld_id,test$date_time),]
  
  #Free space
  rm(wtdata)
  
  #Backup ot/alarm id
  if('ot_block_code' %in% names(test)){
    test_ot_block_code<-test[,c('ld_id','ot_block_code','date_time')]
  }
  
  if('alarm_block_code' %in% names(test)){
    test_alarm_block_code<-test[,c('ld_id','alarm_block_code','date_time')]
  }
  
  if('ot_all' %in% names(test)){
    test_ot_all<-test[,c('ld_id','ot_all','date_time')]
    test_ot<-test[,c('ld_id','ot','date_time')]
    if(!is.null(test_seconds_ot_marging)&&!is.na(test_seconds_ot_marging)&&test_seconds_ot_marging>0){
      others_ots_dt<-unique(test[((test$ot_all==1)&(test$pre_alarm==0)),c('date_time','ld_id')])
      test_ot_all_marging<-lapply(1:nrow(others_ots_dt),function(i) data.frame(ld_id=others_ots_dt$ld_id[i],date_time=seq.POSIXt(others_ots_dt$date_time[i]-as.difftime(test_seconds_ot_marging, units="secs"),by=paste0(seconds_to_aggregate,' sec'),others_ots_dt$date_time[i]+as.difftime(test_seconds_ot_marging, units="secs"),tz='UTC',origin='1970-01-01')))
      test_ot_all_marging<-do.call("rbind",test_ot_all_marging)
      test_ot_all_marging<-unique(test_ot_all_marging)
      test_ot_all_marging$date_time<-as.POSIXct(test_ot_all_marging$date_time,tz = 'UTC',origin='1970-01-01')
      test_excluded_by_marging<-NULL
      for(ld in unique(test_ot_all_marging$ld_id)) test_excluded_by_marging<-c(test_excluded_by_marging,which((test$pre_alarm==0)&(test$ld_id==ld)&(test$date_time %in% test_ot_all_marging$date_time[test_ot_all_marging$ld_id==ld])))
      if(length(test_excluded_by_marging)>0) test<-test[-test_excluded_by_marging,]
    }
    test$ot_all<-NULL
  }
  if('alarm_all' %in% names(test)){
    test_alarm_all<-test[,c('alarm_all','ld_id','date_time')]
    test$alarm_all<-NULL
  }
  
  if('health_status' %in% names(test)){
    test_health_status<-test[,c('health_status','ld_id','date_time')]
    test$health_status<-NULL
  }
  
  #Use the same normalization of train
  test<-test[,colnames(train)]
  test[,!exclude_columns]<-sapply(colnames(test[,!exclude_columns]),function(c) {(test[,c] - train_stat$mean[train_stat$name==c])/train_stat$sdv[train_stat$name==c]})
  
  if(lstm_format){#Prepare timesteps
    test_lstm<-NULL
    columns<-which(!(colnames(test) %in% c('pre_alarm','pre_alarm_0_anti','ot','ot_all','alarm','alarm_all','ot_block_code','alarm_block_code')))
    columns_names<-colnames(test)[columns]
    mb_per_thread<-500 #Worst case 500MB
    if(Sys.info()[['sysname']]=="Linux"){
      free<-as.numeric(system("awk '/MemAvailable/ {print $2}' /proc/meminfo",intern=TRUE))
    }else{#windows
      free<-as.numeric(gsub(x=paste0(system("wmic OS get FreePhysicalMemory /Value",intern = T),collapse = ''),pattern = '.*=([0-9]+)',replacement = '\\1'))
    }
    if(is.na(free)||is.nan(free)||is.infinite(free)) free<-1048576
    num_threads<-floor((free/1024)/mb_per_thread)
    num_threads<-min(num_threads,nrow(test))
    num_threads<-min(num_threads,parallel::detectCores())
    num_threads<-min(nrow(test),floor(num_threads))
    num_threads<-max(num_threads,1) #If numthreads is 0 force to be 1.
    if(is.null(logfile)||is.na(logfile)){
      cl <- parallel::makePSOCKcluster(num_threads)
    }else{
      cl <- parallel::makePSOCKcluster(num_threads,outfile=logfile)
    }
    doSNOW::registerDoSNOW(cl)
    gc(verbose=F)
    tmp<-bigmemory::as.big.matrix(test)
    mdesc <- describe(tmp)
    test_selected_rows<-1:nrow(test)
    date_time_column<-which('date_time'==colnames(test))
    ld_id_column<-which('ld_id'==colnames(test))
    test_lstm <- foreach(r=seq(1,length(test_selected_rows),num_threads),.init = array(NA,dim=c(length(test_selected_rows),timesteps,length(columns)),dimnames = list(1:length(test_selected_rows),1:timesteps,columns_names)),.packages = 'bigmemory',.verbose = F, .combine = 'comb') %dopar% {
      tr<-bigmemory::attach.big.matrix(mdesc)
      row_max<-min(r+num_threads-1,length(test_selected_rows))
      tmp_array<-array(NA,dim=c(row_max-r+1,timesteps,length(columns)),dimnames = list(r:row_max,1:timesteps,columns))
      tmp_dt<-as.POSIXct(tr[,date_time_column],tz = 'UTC',origin='1970-01-01')
      for(cr in r:row_max){
        selected_ld<-tr[test_selected_rows[cr],ld_id_column]
        is_bad<-(test_selected_rows[cr] %in% bad_selected_rows)
        selected_dates<-seq.POSIXt(from = tmp_dt[test_selected_rows[cr]]-as.difftime(timesteps-1, units="days"),by=paste0(seconds_to_aggregate,' sec'),to =tmp_dt[test_selected_rows[cr]],tz='UTC',origin='1970-01-01')
        for(j in 1:length(selected_dates)){
          selected_row<-(selected_dates[j]==tmp_dt)&(tr[,ld_id_column]==selected_ld)
          if(any(selected_row)){#Exists current date
            tmp_array[(cr-r+1),j,]<-tr[selected_row,columns]
          }
        }
      }
      return(list(rmin=r,rmax=row_max,result=tmp_array,timesteps=timesteps))
    }
    stopCluster(cl)
    gc(verbose = F)
    test_lstm_y<-test$pre_alarm_0_anti[1:nrow(test)]
  }
  
  #Save until now to save memory space
  save(list=ls(),file=paste0(tmpfolder,'/after_separation.RData'),compress = 'xz')
  return(list(error=F,data=NULL,msg='ok'))
}