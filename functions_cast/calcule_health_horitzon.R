calcule_health_horitzon<-function(wt_query=NULL,
                                  ld_id=wt_query$ld_id,
                                  health_status=wt_query$health_status,
                                  creation_wtdata_date_end=wt_query$creation_wtdata_date_end,
                                  wtdata=NULL,
                                  horizon=15,
                                  stratification=TRUE,
                                  date_time_name='date_time',
                                  health_status_name='health_status',
                                  factor_unbalance=5){
    iam=match.call()[[1]]
    #For good bad
    id_bad <- numeric(0)
    id_good_final <- integer(0)
    for(i in 1:length(ld_id)){
        #Generate id_bad
        if(!is.na(health_status[i])&&(health_status[i] == 'bad')) {
            date_current <- creation_wtdata_date_end[i]
            id_bad <- c(id_bad, which(wtdata$ld_id == ld_id[i] 
                                      & wtdata[,date_time_name] < date_current 
                                      & wtdata[,date_time_name] >= (date_current - horizon*24*60*60)
                                      & wtdata[,health_status_name] == 'bad') )
        }
    }
    
    #Finally stratification
    if(length(id_bad)>0&&stratification) {
        month_distribution <- table( month(wtdata[id_bad,date_time_name]) )
        bad_months_proportion <- month_distribution / sum(month_distribution)
        bad_months <- names(month_distribution)
        id_good <- which(wtdata[,health_status_name] == 'good' & month(wtdata[,date_time_name]) %in% as.numeric(bad_months))
        total_good <- factor_unbalance*length(id_bad) # length(id_good)
        for( bm in bad_months ) {
            id_good_tmp <- which(wtdata[,health_status_name] == 'good' & month(wtdata[,date_time_name]) == as.numeric(bm))
            id_good_final <- c(id_good_final, sample(id_good_tmp, round(total_good*bad_months_proportion[bm]),replace = TRUE))
        }
    } else {
        id_good <- which(wtdata[,health_status_name] == 'good')
        id_good_final <- sample(id_good, size = factor_unbalance*length(id_bad), replace = TRUE)
    }
    
    return(list(error=FALSE,data=list(id_good=id_good_final,id_bad=id_bad),msg='ok'))
}    