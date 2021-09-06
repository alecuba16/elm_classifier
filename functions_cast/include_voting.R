# include_voting <- function(wtdata, date_time=NULL) {
#     voting <- get_voting(wtdata)
#     if(length(unique(voting)) == 1) {
#         warning("Voting variable not included, always 0 or 1, sum(voting): ", sum(voting, na.rm = T))
#         result <- wtdata
#     } else {
#         # Visual analyze of the voting variable
#         if(is.null(date_time)) x <- 1:nrow(wtdata) else  x <- date_time
#         plot(x, voting)
#         id_pre_alarm <- wtdata[,ncol(wtdata)]==1 | wtdata[,ncol(wtdata)]=='t';
#         points(x[id_pre_alarm], voting[id_pre_alarm],type="p", col="blue", pch=19)
#         # Integrate voting variable to dataset
#         m <- ncol(wtdata)
#         result <- data.frame(wtdata[,1:(m-1)], voting, wtdata[,m])
#     }
#     names(result)[ncol(result)] <- "pre_alarm"
#     return(result)
# }

include_voting <- function(wtdata, date_time=NULL, voting = get_voting(wtdata),target='alarm',disp=FALSE) {
#     if(length(unique(voting)) == 1) {
#         warning("Voting variable not included, always 0 or 1, sum(voting): ", sum(voting, na.rm = T))
#         result <- wtdata
#     } else {
        # Visual analyze of the voting variable
        if(is.null(date_time)) x <- 1:nrow(wtdata) else  x <- date_time
        if(disp) plot(x, voting)
        id_pre_alarm <- wtdata[,target]==1 | wtdata[,target]=='t';
        if(disp) points(x[id_pre_alarm], voting[id_pre_alarm],type="p", col="blue", pch=19)
        # Integrate voting variable to dataset
        m <- ncol(wtdata)
        result <- data.frame(wtdata[,which(names(wtdata)!=target)], voting, wtdata[,target])
        names(result)[ncol(result)] <- target
    # }
    names(result)[which(names(result)==target)] <- "pre_alarm"
    return(result)
}
