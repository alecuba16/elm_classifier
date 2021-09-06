process_datetimeoff <- function(datetimeoff, sampling_period = 600) {
    dto <- datetimeoff[1]
    datetimeoff <- na.omit(datetimeoff)
    for(i in 2:length(datetimeoff)) {
        past <- datetimeoff[i - 1]
        current <- datetimeoff[i]
        if( current > (past + sampling_period) )
            dto <- c(dto, current)
    }
    return(dto)
}