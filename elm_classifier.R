#Clean all
rm(list = ls())
before_vars<-ls()

iam='main_fs'
#Dependencia basica
if(!exists("dependencyLoader")){
  if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
  source('functions_common/dependencyLoader.R')
}

# Sources
# devtools::install_github("rstudio/keras")
# install_keras()
libraries<-c('plotly','ggplot2','xtable','keras','tensorflow','RColorBrewer','RMySQL')
sources_common<-paste0("functions_common/",c('formatter_get_tableinfo.R','db_get_event_description.R','load_wtdata.R','close_protocol.R','db_query.R','filter_custom.R','feature_selection.R'))
sources_cast<-paste0("functions_cast/",c('check_acciona_rules.R','check_acciona_rules_cm3.R'))
dep<-dependencyLoader(c(libraries,sources_common,sources_cast))
if(dep$error)  stop(paste0("\n",iam,":on call dependencyLoader\n",dep$msg))
debug_mode<-FALSE

db_config<-data.frame(user="user",password="password",dbname="yourHistoricalBD",host="127.0.0.1",port=3306)
#db_config<-data.frame(user="user",password="password",dbname="yourHistoricalBD",host="yourHost",port=3306)

id<-2  #id model 12 izco gen day disptemp



query<-paste0('select * from 1_cast_config_compatible where id=',id)
rs<-db_query(query=query,db_config=db_config)
if(rs$error)  stop(paste0("\n",iam,":on call db_query\n",dep$msg))
wt_query<-rs$data

anticipation<-ifelse(is.null(wt_query$anticipation[1]),0,wt_query$anticipation[1])
use_alarms<-grepl(pattern = 'alarm',x = wt_query$target_name[1])
use_ots<-grepl(pattern = 'ot',x = wt_query$target_name[1])
use_health<-grepl(pattern = 'health',x = wt_query$target_name[1])
date_time_name<-'date_time'
tmpfolder<-'tmp'
ifelse(!dir.exists(tmpfolder), dir.create(tmpfolder), FALSE)

rs  <-  load_wtdata(wt_query=wt_query,
                    date_time_name=date_time_name,
                    target_name=NULL,
                    filter_exclude=paste(date_time_name,"nrecords,ot_block_code,ot_all,ot_all_block_code,production,weekly_production,ld_id,alarm,alarm_block_code,alarm_all,alarm_all_block_code,ot,ot_block_code,ot_all,ot_all_block_code,n1,weekly_n1,weekly_power",sep=","),
                    update_filter_ranges=F,
                    filter_verbose=F,
                    db_config=db_config)
if(rs$error) stop(paste0("\n",iam,":on call load_wtdata\n\t",rs$msg))

wtdata<-rs$data$wtdata
outliers<-rs$data$outliers
rm(rs)
target_name<-wt_query$target_name[1]

#OT block code code to string
if('ot_block_code' %in% names(wtdata)){
  #Get descriptions
  codes<-unique(unlist(strsplit(wtdata$ot_block_code,split = ',')))
  rs<-formatter_get_tableinfo(table_cast_park_dic='1_cast_park_table_dic',wp_id=wt_query$wp_id[1],db_config=db_config)
  if(rs$error) stop('downloading formatter_get_tableinfo')
  ot_table_name_dic<-paste0(rs$data$ot_table_name,'_dic')
  rs<-db_get_event_description(paste0(codes,collapse = ','), ot_table_name_dic,all_info=TRUE, target = "ot", db_config=db_config)
  ot_desc<-rs
  #Replace id with descriptions
  for(c in codes){
    wtdata$ot_block_code<-gsub(wtdata$ot_block_code,pattern = c,replacement = ot_desc$descripcio_walm[c==ot_desc$id_ot])
  }
}

if('ot_all_block_code' %in% names(wtdata)){
  #Get descriptions
  codes<-unique(unlist(strsplit(wtdata$ot_all_block_code,split = ',')))
  rs<-formatter_get_tableinfo(table_cast_park_dic='1_cast_park_table_dic',wp_id=wt_query$wp_id[1],db_config=db_config)
  if(rs$error) stop('downloading formatter_get_tableinfo')
  ot_table_name_dic<-paste0(rs$data$ot_table_name,'_dic')
  rs<-db_get_event_description(paste0(codes,collapse = ','), ot_table_name_dic,all_info=TRUE, target = "ot", db_config=db_config)
  ot_desc<-rs
  #Replace id with descriptions
  for(c in codes){
    wtdata$ot_all_block_code<-gsub(wtdata$ot_all_block_code,pattern = c,replacement = ot_desc$descripcio_walm[c==ot_desc$id_ot])
  }
}

#alarm block code to string
if('alarm_block_code' %in% names(wtdata)){
  #Get descriptions
  codes<-unique(unlist(strsplit(wtdata$alarm_block_code,split = ',')))
  rs<-formatter_get_tableinfo(table_cast_park_dic='1_cast_park_table_dic',wp_id=wt_query$wp_id[1],db_config=db_config)
  if(rs$error) stop('downloading formatter_get_tableinfo')
  alarm_table_name_dic<-paste0(rs$data$alarm_table_name,'_dic')
  rs<-db_get_event_description(paste0(codes,collapse = ','), alarm_table_name_dic,all_info=TRUE, target = "alarm", db_config=db_config)
  alarm_desc<-rs
  #Replace id with descriptions
  for(c in codes){
    wtdata$alarm_block_code<-gsub(wtdata$alarm_block_code,pattern = c,replacement = alarm_desc$descripcio_walm[c==alarm_desc$id_ot])
  }
}

#alarm block code to string
if('alarm_all_block_code' %in% names(wtdata)){
  #Get descriptions
  codes<-unique(unlist(strsplit(wtdata$alarm_all_block_code,split = ',')))
  rs<-formatter_get_tableinfo(table_cast_park_dic='1_cast_park_table_dic',wp_id=wt_query$wp_id[1],db_config=db_config)
  if(rs$error) stop('downloading formatter_get_tableinfo')
  alarm_table_name_dic<-paste0(rs$data$alarm_table_name,'_dic')
  rs<-db_get_event_description(paste0(codes,collapse = ','), alarm_table_name_dic,all_info=TRUE, target = "alarm", db_config=db_config)
  alarm_desc<-rs
  #Replace id with descriptions
  for(c in codes){
    wtdata$alarm_all_block_code<-gsub(wtdata$alarm_all_block_code,pattern = c,replacement = alarm_desc$descripcio_walm[c==alarm_desc$id_ot])
  }
}

to_save<-ls()[sapply(ls(),function(var) !is.function(get(var)))]
to_save<-to_save[!(to_save %in% before_vars)]
#to_save<-c('wtdata','wt_query','outliers','date_time_name','query','anticipation','use_alarms','use_ots','use_health','tmpfolder','target_name')
save(list=to_save,file=paste0(tmpfolder,'/wtdata_',id,'_',wt_query$wp_code[1],'_',wt_query$seconds_to_aggregate[1],'s_ant',anticipation,ifelse(use_health,'_health',''),ifelse(use_alarms,'_alarms',''),ifelse(use_ots,'_ots',''),'.RData'),compress = 'xz')
rm(list=to_save)
gc(verbose = F)

############################## Train Test Separation ###################################
#Por temperatura izco
#Grupo1 nov,dic,Enero,febrero,marzo,abril,mayo Grupo 2 Junio,julio,agosto,sept,oct
#wtdata2<-wtdata[,'date_time',drop=F] %>% mutate(month = format(date_time,"%m"))
#wtdata2$month<-as.numeric(wtdata2$month)
#wtdata_inv<-wtdata[wtdata2$month %in% c(1,2,3,4,5,11,12),]

if(!exists('tmpfolder',inherits = F)) tmpfolder='tmp'
before_vars<-ls()
tmp_env<-new.env()
load(file =paste0(tmpfolder,'/wtdata.RData') ,envir = tmp_env)
wtdata<-tmp_env$wtdata
target_name<-tmp_env$target_name
anticipation<-tmp_env$anticipation
use_alarms<-tmp_env$use_alarms
use_ots<-tmp_env$use_ots
use_health<-tmp_env$use_health
seconds_to_aggregate<-tmp_env$wt_query$seconds_to_aggregate[1]
if(!is.null(tmp_env$wt_query$train_machines[1]) && grepl(pattern = '[-]?[0-9]+,',x = tmp_env$wt_query$train_machines[1])){
  train_machines<-as.numeric(unlist(strsplit(tmp_env$wt_query$train_machines[1],split = ',')))
}else{
  train_machines<-tmp_env$wt_query$train_machines[1]
}

balance<-tmp_env$wt_query$balance_per[1]
rm(tmp_env)
gc(verbose = F)

#train_machines=c(121,123,133,131) #Escamb
#Izco Gbox
#Izco seleccionadas con alarmas 607,608,613,627,631,658,659 
#train con fallo 172,194,195,200 
#train bien  168, 171,179?,182?,184?,189?,203?
#train_machines<-c(168,171,172,179,182,184,189,194,195,200,203) #izco specific lds


#Izco Gen
#Peores 196,184,168,170 mejores 175,182,186,216
#train_machines<-c(-196,-184,-168,-170,175,182,186,216)
#train_machines<-70 #50% for train 50% for test.

#Escamb gbox
#train_machines<-c(121,-128,131,123,119) #50% for train 50% for test.
#use_health<-F
#use_ots<-T
#balance<-5 #5%
#balance<-NULL

#Izco gen cambio gen id 14
#Prueba1 izco
#Peores 180,168,171,178,191,209 mejores 167,174,184,192,198
#train_machines<-c(180,168,171,178,191,209,167,174,192,198)
#Prueba2 izco
#train_machines<-c(-169,-170,-179,-189,-190,-212,-178,-201,175,183,186,188,207,210,214,215)


#train_machines<-50 #50% for train 50% for test.
#balance<-1 #1%

if(!exists('train_test_separation')) source('train_test_separation.R')
rs<-train_test_separation(train_machines=train_machines,
                          wtdata=wtdata,
                          target_name=target_name,
                          use_alarms=use_alarms,
                          use_ots=use_ots,
                          use_health=use_health,
                          anticipation=anticipation,
                          seconds_to_aggregate=seconds_to_aggregate,
                          train_seconds_ot_marging=1*86400,
                          test_seconds_ot_marging=1*86400,
                          tmpfolder='tmp_fs',
                          balance=balance,
                          registers_before_pre_alarm=F,
                          select_good_same_bad_interval=T)

if(rs$error) stop(paste0("\n",iam,":on call train_test_separation\n\t",rs$msg))  

rm(list=ls()[!(ls() %in% before_vars)])
gc(verbose = F)
############################################ Feature selection #################################################
if(!exists('tmpfolder',inherits = F)) tmpfolder='tmp'
before_vars<-ls()
tmp_env<-new.env()
load(file =paste0(tmpfolder,'/after_separation.RData') ,envir = tmp_env)
train_selected_rows<-tmp_env$train_selected_rows
target_name<-tmp_env$target_name
target_name<-'pre_alarm'
anticipation<-tmp_env$anticipation
balance<-tmp_env$balance
real_balance<-round(tmp_env$real_balance,digits = 0)
pre_alarm<-tmp_env$pre_alarm
rm(tmp_env)
gc(verbose = F)

tmp_env<-new.env()
load(file =paste0(tmpfolder,'/wtdata.RData') ,envir = tmp_env)
wtdata<-tmp_env$wtdata
rm(tmp_env)
gc(verbose = F)

wtdata$pre_alarm<-pre_alarm$pre_alarm
normalize<-T

#Src,libs
source('functions_common/feature_selection.R', echo=TRUE)

fs_params = list()
#fs_params = rbind(fs_params,list(algorithm='pvalue',na_columns_per_exclude=10,complete_cases=T,params=data.frame(method="simulation",nsim=1000,pvalue=0.05,stringsAsFactors = F),cor_threshold=0.95))
#fs_params = rbind(fs_params,list(algorithm='caretrfe',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.5),cor_threshold=0.95))
#fs_params = rbind(fs_params,list(algorithm='boruta',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.5,maxRuns=50,field='meanImp'),cor_threshold=0.95))
fs_params = rbind(fs_params,list(algorithm='cmim',na_columns_per_exclude=20,complete_cases=T,params=data.frame(q=0.5),cor_threshold=0.95))
#fs_params = rbind(fs_params,list(algorithm='disr',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.5),cor_threshold=0.95))
#fs_params = rbind(fs_params,list(algorithm='jmi',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.5),cor_threshold=0.95))
#fs_params = rbind(fs_params,list(algorithm='jmim',na_columns_per_exclude=20,complete_cases=T,params=data.frame(q=0.5),cor_threshold=0.95))
#fs_params = rbind(fs_params,list(algorithm='mim',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.5),cor_threshold=0.95))
#fs_params = rbind(fs_params,list(algorithm='mrmr',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.5),cor_threshold=0.95))
#fs_params = rbind(fs_params,list(algorithm='njmim',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.5),cor_threshold=0.95))
fs_results<-list()
for(i in 1:nrow(fs_params)){
  cat(paste0('Algoritm ',i,' of ',nrow(fs_params),' :',fs_params[i,]$algorithm,'\n'))
  #Remove all data with ot_all=1 for train
  rs<-feature_selection(wtdata=wtdata[train_selected_rows,],
                        exclude_columns=c('date_time','pre_alarm_0_anti',target_name,'ld_id','health_status','alarm','ot','ot_all','ot_block_code','ot_all_block_code','alarm_block_code','alarm_all_block_code'),
                        target_name=target_name,
                        algorithm=fs_params[i,]$algorithm,
                        date_time_name='date_time',
                        cor_threshold=fs_params[i,]$cor_threshold,
                        na_columns_per_exclude=fs_params[i,]$na_columns_per_exclude,
                        complete_cases=fs_params[i,]$complete_cases,
                        params=fs_params[i,]$params,
                        discretize=F,
                        parallel=T,
                        normalize=normalize,
                        logfile='cluster.log')
  if(rs$error) stop(paste0("\n",iam,":on call feature_selection\n\t",rs$msg))
  tmp<-rs$data
  tmp$wtdata<-NULL #Free space
  tmp$algorithm<-fs_params[[i,'algorithm']]
  tmp$params<-fs_params[[i,'params']]
  tmp$complete_cases<-fs_params[[i,'complete_cases']]
  tmp$na_columns_per_exclude<-fs_params[[i,'na_columns_per_exclude']]
  tmp$cor_threshold<-fs_params[[i,'cor_threshold']]
  fs_results<-rbind(fs_results,tmp)
  rm(rs)
  gc(verbose = F)
}

library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)

##ALL the scores
#Compare selections
scores<-NULL
x_cut<-NULL
x<-NULL
for(i in 1:nrow(fs_results)){
  tmp<-fs_results[[i,'scores_variables']]
  algo_name<-fs_results[[i,'algorithm']]
  if(algo_name=='pvalue'){
    value<-1-(round((max(tmp$score[tmp$selected],na.rm = T)-min(tmp$score,na.rm = T))/(max(tmp$score,na.rm = T)-min(tmp$score,na.rm = T)),digits = 3))
  }else{
    value<-round((min(tmp$score[tmp$selected],na.rm = T)-min(tmp$score,na.rm = T))/(max(tmp$score,na.rm = T)-min(tmp$score,na.rm = T)),digits = 3)
  }
  x_cut<-c(x_cut,paste0(algo_name,'>',value))
  x<-c(x,algo_name)
  
  if(!is.null(scores)){
    scores<-merge(scores,tmp[,c('name','score')],by = 'name',all=T)
    colnames(scores)<-c('name',unlist(fs_results[,'algorithm'])[1:(ncol(scores)-1)])
  }else{
    scores<-tmp[order(tmp$name),c('name','score')]
    colnames(scores)[2]<-algo_name
  }
  if(length(unique(scores$name))!=length(scores$name)) cat('\n i:',i,' duplicated name \n')
}

#Normalize scores
norm_scores<-scores
norm_scores[,2:(nrow(fs_results)+1)]<-apply(norm_scores[,2:(nrow(fs_results)+1),drop=F],2,function(c) (c-min(c,na.rm = T))/(max(c,na.rm = T)-min(c,na.rm = T)))
pvalue_pos<-which(unlist(fs_results[,'algorithm'])=='pvalue')
if(length(pvalue_pos)>0){#Invert Pvalue
  norm_scores[,pvalue_pos]<-(1-norm_scores[,pvalue_pos])
}
rownames(norm_scores)<-norm_scores[,1]
norm_scores[,1]<-NULL
p<-plot_ly(x=x_cut,y=rownames(norm_scores),z=round(as.matrix(norm_scores),digits = 3),type='heatmap')%>% layout(title = 'All variables', xaxis = list(title='Algorithm and cut value'),yaxis = list(title='Variable'), margin=list(l=150))
htmlwidgets::saveWidget(p,file = paste0(getwd(),'/',tmpfolder,'/fs_comparison_allscores_a',anticipation,'_',ifelse(is.null(balance),'no_balanced',paste0('b',balance)),'.html'),libdir = paste0(getwd(),'/',tmpfolder,'/lib'),selfcontained = F)

#Only selected
#Compare selections
scores<-NULL
x_cut<-NULL
x<-NULL
for(i in 1:nrow(fs_results)){
  tmp<-fs_results[[i,'scores_variables']]
  algo_name<-fs_results[[i,'algorithm']]
  tmp_scores<-tmp$score[tmp$selected]
  x<-c(x,algo_name)
  
  if(!is.null(scores)){
    scores<-merge(scores,tmp[tmp$selected,c('name','score')],by = 'name',all=T)
    colnames(scores)<-c('name',unlist(fs_results[,'algorithm'])[1:(ncol(scores)-1)])
  }else{
    scores<-tmp[tmp$selected,c('name','score')]
    scores<-scores[order(scores$name),]
  }
}
#Normalize scores
norm_scores<-scores
norm_scores[,2:(nrow(fs_results)+1)]<-apply(norm_scores[,2:(nrow(fs_results)+1),drop=F],2,function(c) (c-min(c,na.rm = T))/(max(c,na.rm = T)-min(c,na.rm = T)))
pvalue_pos<-which(unlist(fs_results[,'algorithm'])=='pvalue')
if(length(pvalue_pos)>0){#Invert Pvalue
  norm_scores[,pvalue_pos]<-(1-norm_scores[,pvalue_pos])
}
rownames(norm_scores)<-norm_scores[,1]
norm_scores[,1]<-NULL
p<-plot_ly(x=x,y=rownames(norm_scores),z=round(as.matrix(norm_scores),digits = 3),type='heatmap') %>% layout(title = 'Selected variables', xaxis = list(title='Algorithm'),yaxis = list(title='Variable'), margin=list(l=150)) 
htmlwidgets::saveWidget(p,file = paste0(getwd(),'/',tmpfolder,'/fs_comparison_selectedscores_a',anticipation,'_',ifelse(is.null(balance),'no_balanced',paste0('b',balance)),'.html'),libdir = paste0(getwd(),'/',tmpfolder,'/lib'),selfcontained = F)

#Save until now to save memory space
save(list=c('fs_results','fs_params'),file=paste0(tmpfolder,'/after_feature.RData'),compress = 'xz')
rm(list=ls()[!(ls() %in% before_vars)])
gc(verbose = F)

################################### CLASS benchmark #########################################
if(!exists('tmpfolder',inherits = F)) tmpfolder='tmp'
before_vars<-ls()
tmp_env<-new.env()
load(file =paste0(tmpfolder,'/after_separation.RData') ,envir = tmp_env)
train_selected_rows<-tmp_env$train_selected_rows
test_selected_rows<-tmp_env$test_selected_rows
pre_alarm<-tmp_env$pre_alarm
pre_alarm_0_anti<-tmp_env$pre_alarm_0_anti
real_balance<-round(tmp_env$real_balance,digits = 0)
use_ots<-tmp_env$use_ots
balance<-tmp_env$balance
use_ots<-tmp_env$use_ots
use_alarms<-tmp_env$use_alarms
anticipation<-tmp_env$anticipation
rm(tmp_env)
tmp_env<-new.env()
load(file =paste0(tmpfolder,'/after_feature.RData') ,envir = tmp_env)
fs_results<-tmp_env$fs_results
rm(tmp_env)

tmp_env<-new.env()
load(file =paste0(tmpfolder,'/wtdata.RData') ,envir = tmp_env)
wtdata<-tmp_env$wtdata
rm(tmp_env)

wtdata$pre_alarm<-pre_alarm$pre_alarm

if(!exists("create_elm_model",inherits = F)) source('create_elm_model.R')
if(!exists("model_indicators")) source('model_indicators.R')
if(!exists("confusion_matrix")) source('confusion_matrix.R')
if(!exists("check_acciona_rules")) source('functions_cast/check_acciona_rules.R')
if(!exists("check_acciona_rules_cm3")) source('functions_cast/check_acciona_rules_cm3.R')

class_results<-list()
cm_bench_train<-list()
cm_bench_test<-list()

cmim<-which(unlist(fs_results[,'algorithm'])=='cmim')
fs_algo<-fs_results[[cmim,'algorithm']]
selected<-fs_results[[cmim,'selected']]

rs<-create_elm_model(data=wtdata[train_selected_rows,c(selected,'pre_alarm')],K=10,target_column_name='pre_alarm',date_time_name='date_time',activation=F,pseudo_inverse=T,classifier=T)
if(rs$error) stop(paste0('Error in create_elm_model in algo ',fs_algo))
model<-rs$data$model
#Predict all the data of train
train<-wtdata[wtdata$ld_id %in% unique(wtdata$ld_id[train_selected_rows]),]
rs<-model$predict(model = model,data = train[,selected])
pred_train=data.frame(algorithm=fs_algo,date_time=train$date_time,yhat=rs$data$yhat)
#Predict test
test<-wtdata[test_selected_rows,]
rs<-model$predict(model=model,X=test[,selected],normalize = F)
pred_test=data.frame(algorithm=fs_algo,date_time=test$date_time,yhat=rs$data$yhat,yhat=rs$data$yhat)
class_results<-rbind(class_results,list(algorithm=fs_algo,target_ratio=target_ratio,architecture=architecture_bench[[i,'architecture']],activation=architecture_bench[[i,'activation']],dropout=architecture_bench[[i,'dropout']],pred_train=pred_train,pred_test=pred_test,history=rs$data$history,stat=rs$data$stat,zero_sdv_columns_names=rs$data$zero_sdv_columns_names))
    
if(is.null(wtdata_train)) wtdata_train<-train[train$date_time %in% class_results[[i,'pred_train']]$date_time,]
if(is.null(wtdata_test)) wtdata_test<-test[test$date_time %in% class_results[[i,'pred_test']]$date_time,]

rs<-model_indicators(fun_cm=check_acciona_rules,wtdata=wtdata_train,target_ratio=target_ratio,architecture=architecture_bench[[i,'architecture']],dropout=architecture_bench[[i,'dropout']],activation=architecture_bench[[i,'activation']],class_results=class_results[i,'pred_train'],train_ot_all,use_ots,use_alarms,type='train',generate_cm_table=F,generate_plots=T,threshold=0.5)
if(rs$error) stop(paste0('Error in model_indicators'))

rs<-model_indicators(fun_cm=check_acciona_rules,wtdata=wtdata_test,target_ratio=target_ratio,architecture=architecture_bench[[i,'architecture']],dropout=architecture_bench[[i,'dropout']],activation=architecture_bench[[i,'activation']],class_results=class_results[i,'pred_test'],test_ot_all,use_ots,use_alarms,type='test',generate_cm_table=F,generate_plots=T,threshold=0.5)
if(rs$error) stop(paste0('Error in model_indicators'))