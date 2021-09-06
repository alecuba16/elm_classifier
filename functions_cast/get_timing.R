get_timing <- function(da, on = 1, off = 0, state_name = "activ_st", date_time_name = "date_time") {
    da2 <- da[FALSE,]
    da2$date_time_end <- as.POSIXct(character())
    da2$length <- numeric()

    j=1
    while( j < nrow(da) ) {
        if(da[j,state_name] == on & da[j+1,state_name] == off) {
            da2 <- rbind(da2, cbind(da[j,], date_time_end = da[j+1, date_time_name], 
                                    length = difftime( da[j+1,date_time_name], da[j,date_time_name], units = "mins" ) ) )
        } else if( da[j,state_name] == on & da[j+1,state_name] == on ) {
            da2 <- rbind(da2, cbind(da[j,], date_time_end = as.POSIXct(NA), length = difftime(as.POSIXct(NA), as.POSIXct(NA), units = "mins")))
        }
        j=j+1
    } # This while only evaluates till nrow(da)-1
    # Now the evaluation when j == nrow(da)
    if( da[j,state_name] == on )
        da2 <- rbind(da2, cbind(da[j,], date_time_end = as.POSIXct(NA), length = difftime(as.POSIXct(NA), as.POSIXct(NA), units = "mins")))

    return(da2)
}

# Old version before adaptation with historical data base format
# get_timing <- function(da, on = 1, off = 0) {
#     da2 <- da[FALSE,]
#     da2$date_end <- as.POSIXct(character())
#     da2$length <- numeric()
#     
#     j=1
#     while( j < nrow(da) ) {
#         if(da$activ_st[j] == on & da$activ_st[j+1] == off) {
#             da2 <- rbind(da2, cbind(da[j,], date_end = da[j+1, "date"], 
#                                     length = difftime( da[j+1,"date"], da[j,"date"], units = "mins" ) ) )
#         } else if( da$activ_st[j] == on & da$activ_st[j+1] == on ) {
#             da2 <- rbind(da2, cbind(da[j,], date_end = as.POSIXct(NA), length = difftime(as.POSIXct(NA), as.POSIXct(NA), units = "mins")))
#         }
#         j=j+1
#     }
#     return(da2)
# }