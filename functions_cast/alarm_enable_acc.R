# Script to build alarm_enable variable (column) in ACCIONA's park events.

# Libraries and sources
library(dplyr)
library(RMySQL)
source('functions_common/db_query.R')

## Initialization
db_config_data<- data.frame(user='user',
                            password='password',
                            dbname='yourHistoricalBD',
                            host='yourHost',
                            port=3306,
                            stringsAsFactors = F)
automatic_host <- TRUE
if(automatic_host && Sys.info()["nodename"] == "USUARI-PC")
    db_config_data$host <- '127.0.0.1'

events_table_name <- "escamb_awp3k_events"
date_time_name <- "date_time"
date_start <- "2014-01-01"
date_end <- "2017-01-01"
disable_id_walm_dic <- c(765,804,1835,1850,5561,5552,5553,5556,5557) # event codes for maintenance or problem in the WT in Escambrons

## Read alarm table
query <- paste0("SELECT * FROM ",events_table_name," WHERE ",date_time_name," BETWEEN '",date_start,"' AND '",date_end,"';")
rs  <- db_query(query, db_config = db_config_data)
if(rs$error) 
    stop("Error reading events data: ", rs$msg)
dfa <- rs$data

## Process alarm table
head(dfa)
dfa <- arrange(dfa, ld_id, date_time, desc(status))
tail(dfa)

check_disable_alarms <- function(x) {
    x$alarm_enable <- integer(nrow(x))
    current_status <- 1
    for(i in 1:nrow(x)) {
        if( current_status == 1 && x$id_walm[i] %in% disable_id_walm_dic && x$status[i] == 'ON') {
            current_status <- 0
        }
        if( current_status == 0 && x$id_walm[i] %in% disable_id_walm_dic && x$status[i] == 'OFF') {
            current_status <- 1
        }
        x$alarm_enable[i] <- current_status
    }
    x
}
# Test
# x <- dfa[dfa$ld_id == 119,]
# xx <- check_disable_alarms(x)
# head(xx, 100)
# execution
dfa_list <- split(dfa,dfa$ld_id)
dfa_list <- lapply(dfa_list, FUN = check_disable_alarms)
dfax <- data.frame(do.call("rbind", dfa_list))    

# Update alarm table with new variable
x <- dfax
cat("\nUpdating ",events_table_name,". Number of events = ", nrow(x),".\n", sep = "")
query <- paste("UPDATE ", events_table_name, " SET alarm_enable = ",x$alarm_enable, " WHERE id = ",x$id,";",sep = "")
con <- dbConnect(MySQL(), user=db_config_data$user, password=db_config_data$password,dbname=db_config_data$dbname, host=db_config_data$host, port=db_config_data$port)

for(i in 1:length(query)) {
    # db_query(query[i], db_config = db_config_data)
    rs <- dbSendQuery(con, query[i])
    if( i %% 1000 == 0) {
        cat(format(i/length(query)*100, digits = 4),"\n",sep = "")
    }
}
dbDisconnect(con)
