vector_to_events <- function(date_time, state, on = "ON", off = "OFF") {
    
    # Bags to store start and end date of the event
    date_start <- as.POSIXct(character())
    date_end <- as.POSIXct(character())
    # Delete other levels in "state"
    id_sel <- which(state == on | state == off)
    date_time <- date_time[id_sel]
    state <- state[id_sel]
    
    # If first is ON, assume last as OFF. This is the first start date
    if(state[1] == on)
        date_start <- c(date_start, date_time[1])
    
    for(i in 2:length(state)) {
        current <- state[i]
        last <- state[i-1]
        if( any(is.na(c(current, last))) ) next
        if( current == on & last == off ) 
            date_start <- c(date_start, date_time[i])
        if( current == off & last == on ) 
            date_end <- c(date_end, date_time[i])
    }
    
    # If finish in ON, assume current (last date_time) as OFF
    if(length(date_start) - length(date_end) == 1)
        date_end <- c(date_end, date_time[length(date_time)])    
    
    if(length(date_start) - length(date_end) != 0)
        stop("date_start has different length of date_end. Abort process")
    
    events <- data.frame(date_start = date_start, date_end = date_end, length = date_end - date_start)
    return(events)
}