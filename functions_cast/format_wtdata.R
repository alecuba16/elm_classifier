format_wtdata <- function(wtdata0, horizon = 28, 
                          type = ifelse(is.null(wt_query), "phealth",wt_query$type), 
                          wt_query = NULL, anticipation = 14, force_forecast = FALSE,target='alarm',db_config) {
    source('functions_cast/get_weather_forecast.R')
    source('functions_cast/get_unhealthy.R')
	iam="format_wtdata"
    # For debugging
    # load("wtdata0.RData")
    # horizon = 28
    # type="pfailure"
    # anticipation = 14
    # wt_query <- data.frame(wp_code = "rob_tr", ld_id = 107, stringsAsFactors = FALSE)
    # phealth_name <- "phealth"
    # debug_mode <- TRUE
    # End debugging
    
    # Format wtdata and build unhealthy vector in alarma vector
    ###########
    wtdata <- wtdata0
	if( !(target %in% names(wtdata)) ) return(list(error=TRUE,data=NULL,msg=paste0(iam,":Target variable ",target," doesn't exists in wtdata")))
    names(wtdata)[which(names(wtdata) == target)] <-'alarm'
	wtdata1 <- wtdata # Save copy in case it is needed to get id_no_inclu
    id_u <- get_unhealthy(wtdata, h = horizon)
    wtdata$alarm[id_u] <- 1
    if(type == "phealth" || type == phealth_name) {
		return(list(error=FALSE,data=wtdata,msg="ok"))
    } else if(type == "pfailure" | type == pfailure_name) {
        id_no_inclu <- get_unhealthy(wtdata1, h = anticipation)
        # wtdata <- wtdata[!id_no_inclu,] 
        wtdata$alarm[id_no_inclu] <- NA # Better use NA's in target instead of deleting rows in wtdata. Therefore the wtdata stays synchronised with wtdata0
        weather_forecast <- get_weather_forecast(wp_code = wt_query$wp_code, ld_id = NULL, 
                                                 horizon_max = 1, date_time_array = as.character(wtdata$datetime1 + anticipation - 1),
                                                 force_forecast = force_forecast,db_config=db_config)
        weather_forecast$datetime1 <- as.Date(weather_forecast$date_time) - anticipation
        wtdata <- merge(wtdata,weather_forecast[,c("datetime1", "wind_avg","wind_max")], by = "datetime1", all.x = TRUE, all.y = FALSE, sort = FALSE)
        wtdata <- dplyr::select(wtdata, -alarm, alarm) # Alarm always at the end of the dataframe.
        return(list(error=FALSE,data=wtdata,msg="ok"))
    } else {
        cat("\nType:", type,"no found\n")
        stop("Error, type", type,"no found.")
    }
}