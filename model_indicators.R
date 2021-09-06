model_indicators<-function(wtdata,target_ratio,architecture=NULL,dropout=NULL,activation=NULL,class_results,fun_cm=NULL,ot_all,use_ots,use_alarms,type='',generate_cm_table=T,generate_plots=T,threshold=0.7){
  
  if(is.null(fun_cm)) return(list(error=T,data=NULL,msg="fun_cm function not set"))
  if(!is.null(ot_all)&&!is.na(ot_all)){
    ot_all$date_time<-as.POSIXct(ot_all$date_time,tz='UTC',origin='1970-01-01')
    ot_all<-ot_all[ot_all$ot_all==T,]
    wtdata<-merge(wtdata,ot_all,by=c('date_time','ld_id'),all = TRUE)
  }
  
  architecture<-paste0(architecture,collapse = '-')
  dropout<-ifelse(!is.null(dropout)&&!is.na(dropout),dropout,'')
  activation<-ifelse(!is.null(activation)&&!is.na(activation),activation,'')
  
  classifierinfo<-paste0(architecture,paste0('d',dropout))
  classifierinfo<-paste0(classifierinfo,paste0('-',activation))
  library(zoo)
  
  cm<-data.frame(ld=as.numeric(),algo=as.character(),acc=as.numeric(),pres=as.numeric(),kappa=as.numeric(),tp=as.numeric(),fp=as.numeric(),tn=as.numeric(),fn=as.numeric(),stringsAsFactors = F)
  for(ld in unique(wtdata$ld_id)){
    current_ld_rows<-which((wtdata$ld_id==ld)&!is.null(wtdata$pre_alarm)&!is.na(wtdata$pre_alarm))
    if(any(wtdata$ld_id==ld)){
      current_pre_alarm<-wtdata$pre_alarm[current_ld_rows] #pre alarm
      
      target<-current_pre_alarm
      if(use_ots&&'ot' %in% names(wtdata)) target<-wtdata$ot[current_ld_rows]
      if(use_alarms&&'alarm' %in% names(wtdata)) target<-target|(wtdata$alarm[current_ld_rows]) #real alarm
      dt_tmp<-wtdata$date_time[current_ld_rows]
      for(j in 1:length(class_results)){
        fs_algo<-class_results[[j]]$algorithm[1]
        yhat<-class_results[[j]]$yhat
        yhat<-yhat[current_ld_rows]
        
        if('ot_all' %in% names(wtdata)) yhat[(wtdata$ot_all[current_ld_rows]==1)&(wtdata$ot[current_ld_rows]==0)]<-0 #Days with other ot probability dropped to 0 because is affected by the work.
        yhat_bin<-(yhat>threshold)
        
        tmp_cm <- fun_cm(date_time = dt_tmp,pre_alarm = yhat_bin, alarm = target, anticipation = 0, margin = 30,critic_fault=F)
        if(tmp_cm$error) return(list(error=T,data=NULL,msg=paste0("\n",iam," on call cm:",tmp_cm$msg)))
        tp<-sum(tmp_cm$data$conf_matrix=='tp',na.rm = T)
        fp<-sum(tmp_cm$data$conf_matrix=='fp',na.rm = T)
        tn<-sum(tmp_cm$data$conf_matrix=='tn',na.rm = T)
        fn<-sum(tmp_cm$data$conf_matrix=='fn',na.rm = T)
        total<-sum(tp+tn+fp+fn)
        acc<-(tp+tn)/total
        pres<-tp/(tp+fp)
        ma<-((tp+fp)*(tp+fn))/total
        mb<-((tn+fp)*(tn+fn))/total
        pe<-(ma + mb)/total
        k<-(acc-pe)/(1-pe)
        cm<-rbind(cm,data.frame(ld=ld,algo=fs_algo,acc=acc,pres=pres,kappa=k,tp=tp,fp=fp,tn=tn,fn=fn))
      }
    }
  }
  #cm$ld<-as.factor(cm$ld)
  kappa_mean<-round(sapply(levels(cm$algo),function(a) mean(cm$kappa[cm$algo==a],na.rm=T)),3)
  kappa_median<-round(sapply(levels(cm$algo),function(a) median(cm$kappa[cm$algo==a],na.rm=T)),3)
  tp_mean<-round(sapply(levels(cm$algo),function(a) mean(cm$tp[cm$algo==a],na.rm=T)),3)
  tp_median<-round(sapply(levels(cm$algo),function(a) median(cm$tp[cm$algo==a],na.rm=T)),3)
  fp_mean<-round(sapply(levels(cm$algo),function(a) mean(cm$fp[cm$algo==a],na.rm=T)),3)
  fp_median<-round(sapply(levels(cm$algo),function(a) median(cm$fp[cm$algo==a],na.rm=T)),3)
  tn_mean<-round(sapply(levels(cm$algo),function(a) mean(cm$tn[cm$algo==a],na.rm=T)),3)
  tn_median<-round(sapply(levels(cm$algo),function(a) median(cm$tn[cm$algo==a],na.rm=T)),3)
  fn_mean<-round(sapply(levels(cm$algo),function(a) mean(cm$fn[cm$algo==a],na.rm=T)),3)
  fn_median<-round(sapply(levels(cm$algo),function(a) median(cm$fn[cm$algo==a],na.rm=T)),3)
  pres_mean<-round(sapply(levels(cm$algo),function(a) mean(cm$pres[cm$algo==a],na.rm=T)),3)
  pres_median<-round(sapply(levels(cm$algo),function(a) median(cm$pres[cm$algo==a],na.rm=T)),3)
  acc_mean<-round(sapply(levels(cm$algo),function(a) mean(cm$acc[cm$algo==a],na.rm=T)),3)
  acc_median<-round(sapply(levels(cm$algo),function(a) median(cm$acc[cm$algo==a],na.rm=T)),3)
  indicators_summary<-data.frame(algo=levels(cm$algo),kappa_mean=kappa_mean,kappa_median=kappa_median,tp_mean=tp_mean,tp_median=tp_median,fp_mean=fp_mean,fp_median=fp_median,tn_mean=tn_mean,tn_median=tn_median,fn_mean=fn_mean,fn_median=fn_median,pres_mean=pres_mean,pres_median=pres_median,acc_mean=acc_mean,acc_median=acc_median)
  
  if(generate_cm_table){
    html_tags <- vector(mode = "list", length = 4)
    html_tags[[1]]<-htmltools::h1(paste0("CM results for a classifier with architecture:",architecture," dropout:",dropout," and target_ratio(ratio of health to alarm):",target_ratio))
    html_tags[[2]]<-DT::datatable(cm,caption = "Confusion Matrix by method and ld_id")
    html_tags[[3]]<-htmltools::br()
    html_tags[[4]]<-DT::datatable(indicators_summary,caption = "Confusion Matrix by method")
    save_html(tagList(html_tags), paste0(getwd(),'/',tmpfolder,'/',type,'_',classifierinfo,'_',target_ratio,'_cm.html'), background = "white", libdir = "lib")
    save(list=c('cm','indicators_summary'),file=paste0(tmpfolder,'/',type,'_',classifierinfo,'_',target_ratio,'_cm.RData'),compress = 'xz')
  }
  
  if(generate_plots){
    for(ld in unique(wtdata$ld_id)){
      df<-NULL
      current_ld_rows<-which(wtdata$ld_id==ld)
      #Selected
      tmp_df<-wtdata[current_ld_rows,]
      
      if('ot_all' %in% names(wtdata)) df<-rbind(df,data.frame(ld_id=ld,date_time=tmp_df$date_time,yhat=tmp_df$ot_all,fs_algo='ot_all',text=paste('date:',as.POSIXct(tmp_df$date_time),'<br>fs_algo:ot_all',stringsAsFactors = F),ot_other=0))
      if('ot' %in% names(wtdata)) df<-rbind(df,data.frame(ld_id=ld,date_time=tmp_df$date_time,yhat=tmp_df$ot,fs_algo='ot',text=paste('date:',as.POSIXct(tmp_df$date_time),'<br>fs_algo:ot',stringsAsFactors = F),ot_other=0))
      if('alarm_all' %in% names(wtdata)) df<-rbind(df,data.frame(ld_id=ld,date_time=tmp_df$date_time,yhat=tmp_df$alarm_all,fs_algo='alarm_all',text=paste('date:',as.POSIXct(tmp_df$date_time),'<br>fs_algo:alarm_all'),ot_other=0,stringsAsFactors = F))
      df<-rbind(df,data.frame(ld_id=ld,date_time=tmp_df$date_time,yhat=tmp_df$pre_alarm,fs_algo=rep('pre_alarm',length(tmp_df$pre_alarm)),text=paste('date:',as.POSIXct(tmp_df$date_time),'<br>fs_algo:pre_alarm'),ot_other=0,stringsAsFactors = F))
      for(j in 1:length(class_results)){
        fs_algo<-class_results[[j]]$algorithm[1]
        yhat<-class_results[[j]]$yhat
        df<-rbind(df,data.frame(ld_id=ld,date_time=tmp_df$date_time,yhat=yhat[current_ld_rows],fs_algo=fs_algo,text=paste('date:',as.POSIXct(tmp_df$date_time),'<br>fs_algo:',fs_algo,'<br>prob:',round(yhat[current_ld_rows],4),'<br>pre_alarm:',(tmp_df$pre_alarm>0),'<br>ot_other:',ifelse(is.na(tmp_df$ot_all>0),0,tmp_df$ot_all>0)),ot_other=ifelse(is.na((tmp_df$pre_alarm==0)&(tmp_df$ot_all>0)),0,(tmp_df$pre_alarm==0)&(tmp_df$ot_all>0)),stringsAsFactors = F))
      }
      
      df$fs_algo<-as.factor(df$fs_algo)  
      #Only shows points with pre_alarms or >0.001.
      df<-df[is.na(df$ot_other)|(!df$ot_other&(df$yhat>0.001)),]
      p<-ggplot(data = df[df$ld_id==df,],mapping = aes(x=date_time,y=yhat,color=fs_algo,text=text))
      p<-p + geom_point(alpha=0.6)+theme_light()+scale_colour_brewer(palette="Paired")
      p<-p + ggtitle(paste0("Results of the turbine ",ld," from ",type," dataset, with classifier architecture:",architecture," dropout:",dropout," and target_ratio(ratio of health to alarm):",target_ratio))
      
      ggsave(paste0(getwd(),'/',tmpfolder,'/',type,'_',target_ratio,'_fs_class_results_',ld,'.png'),plot=p, width = 20, height = 12)
      p<-ggplotly(p,tooltip=c('text'))
      #p<- p+geom_point_interactive(aes(tooltip = text, data_id = text), size = 2) 
      #p<-ggiraph(ggobj =p)
      #htmlwidgets::saveWidget(p,file = paste0(tmpfolder,'/fs_class_results_',ld,'.html'))
      htmlwidgets::saveWidget(p,file = paste0(getwd(),'/',tmpfolder,'/',type,'_',classifierinfo,'_',target_ratio,'_fs_class_results_',ld,'.html'),libdir = paste0(getwd(),'/',tmpfolder,'/lib'),selfcontained = F)
    }
  }
  return(list(error=F,data=list(cm=cm,indicators_summary=indicators_summary,prob_plot=df),msg='ok'))
}