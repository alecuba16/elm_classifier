## This function builds an unhealthy vector from daily data
get_unhealthy <- function(df, h = 14, positive = 1) {
    #df <- wtdata
    #df$alarm[df$date_time=='2015-03-27'] <- positive
    df$healthy <- factor( rep('h', nrow(df)), levels = c('h','u') )
    id_alarm <- which(df$alarm == positive)
    df$healthy[id_alarm] <- 'u'
    # h <- 14
    for( i in id_alarm ) {
        alarm_date <- df$date_time[i]
        for( j in 1:h ) {
            id <- which(df$date_time== (alarm_date - j*24*60*60) ) # TODO: Delete "for" and use idexes of dates between two dates
            if( length(id) >= 1 ) {
                df$healthy[id] <- 'u'
            }
        }
    }
    # Select
    # library(plotly)
    # p <- plot_ly(x = df$date_time, y = df$healthy)
    # p <- add_trace(p = p, x = df$date_time, y = df$alarm)
    # #p <- add_trace(p = p, x = sma$fitmodel, y = sma$Spec)
    # p
    
    id_u <- df$healthy=='u'
    return(id_u)
}

## This function builds an unhealthy vector from minutal data
get_unhealthy_min <- function(df, minutes = 10, h = 1, positive = 1) {
    #df <- wtdata
    #df$alarm[df$date_time=='2015-03-27'] <- positive
    df$healthy <- factor( rep('h', nrow(df)), levels = c('h','u') )
    id_alarm <- which(df$alarm == positive)
    df$healthy[id_alarm] <- 'u'
    # h <- 14
    daily_blocks <- as.integer(24*(60/minutes))
    for( i in id_alarm ) {
        alarm_date <- df$date_time[i]
        for( j in 1:as.integer(h*daily_blocks) ) {
            id <- which( df$date_time == (alarm_date - minutes*j*60) )
            if( length(id) >= 1 ) {
                df$healthy[id] <- 'u'
            }
        }
    }
    # Select
    # library(plotly)
    # p <- plot_ly(x = df$date_time, y = df$healthy)
    # p <- add_trace(p = p, x = df$date_time, y = df$alarm)
    # #p <- add_trace(p = p, x = sma$fitmodel, y = sma$Spec)
    # p
    
    id_u <- df$healthy=='u'
    return(id_u)
}