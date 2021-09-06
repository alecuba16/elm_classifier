# Cargar datos y mostrar información del parque
library(RMySQL)
library(plyr)
library(dplyr)
library(zoo)
library(lubridate)
library(xtable)
library(googleVis)
library(reshape)
source("findnear.R")
source('../functions_common/db_get_event_description.R')
source('../functions_common/get_var_description.R')
source('process_datetimeoff.R')
source('vector_to_events.R')
source('events_to_vector.R')
source('get_timing.R')
source('pretty_difftime.R')
########################################
#### Reading data from Data bases   ####
########################################
# Connect to the DB
con <- dbConnect(MySQL(), user="user", password="password", 
                 dbname="yourHistoricalBD", host="yourHost", port=3306)


# Read Scada data to dfs (data frame scada)
# path = "~/1 Raw data/ROBRES/raw data/"
# file = paste0(path,"robres_all_formated_data4.RData")
# load(file) # Load "df" with all data from 2013-01 to 2015-03

query <- "SELECT * FROM rob_v90" 
rs <- dbSendQuery(con, query)
df <- fetch(rs, n=-1)

dfs <- select(df, id, model, date = date_time, windmin = ambient_windspeed_min, windmax = ambient_windspeed_max, totalpower = total_active_power)
# dfs <- filter( dfs, year(date) %in% c(2014,2015) ) # To keep variable names.
rm(df) 

dbDisconnect(con)
rm(list = c("con", "rs", "query"))

file_patter <- "rob_v90"

########################################
#### Format data frames
########################################
# Normalization rutine (dfa: date, dfs: date, wind, power)
# Specific conversition for ROBRES datasets
dfs <- arrange(dfs, model, date)
diff_model <- function(p){ y <- diff(p)*6; y <- c(y[1], y); y}
ldfs <- split(dfs$totalpower, dfs$model)
ly <- lapply(ldfs, diff_model)
dfy <- unsplit(ly, dfs$model)
dfy[dfy > 2500 | dfy < -500] <- NA
dfs$power_avg <- dfy
# y[y>2200] <- (y[which(y>2200)+1] + y[which(y>2200)-1])/2 # Filter specific outliers
dfs$wind_avg <- (dfs$windmax + dfs$windmin)/2
# Date format
# OJO!: be sure that the datetime field is in GMT time zone

################ INSERT TO DATA BASE #############################
### OJO! This can take too much time, maybe better upload the data to a temporal
### table and then update fields directly from the temporal table.
con <- dbConnect(MySQL(),
                 user="user", password="password",
                 dbname="yourHistoricalBD", host="yourHost",
                 port=3306)

for( i in 2:nrow(dfs) ) {
    power <- dfs$power_avg[i]
    wind <- dfs$wind_avg[i]
    query <- paste0("UPDATE rob_v90 SET power_avg = ", ifelse(is.na(power), 'NULL', power), 
                    ", wind_avg = ",ifelse(is.na(wind), 'NULL', wind)," WHERE id = ", dfs$id[i])
    
    rs <- dbSendQuery(con, query)
}

dbDisconnect(con)

write.csv(dfs[,c("id", "power_avg", "wind_avg")], file = "rob_v90_load_power_wind.csv")

