events_to_vector <- function(date_time, events, period = 600, last_included = T) {
    #library(plyr)
    v <- data.frame( date_time, alarm = factor("OFF", levels = c("OFF","ON")) )
    events[,1] <- round_any(events[,1], period) # Round start date
    events[,2] <- round_any(events[,2], period) # Round end date
    
    for( i in 1:nrow(events) ) {
        # Next iteration if any date in events is NaN
        if( any(is.na(events[i,])) ) next
        # Else:
        if( last_included )
            v$alarm[ date_time >= events[i,1] & date_time <= events[i,2] ] <- "ON"
        else 
            v$alarm[ date_time >= events[i,1] & date_time < events[i,2] ] <- "ON"
    }
    return(v)
}