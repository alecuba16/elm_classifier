load_scada_wtdata <- function(ld_id = 126, wp_code = "escamb", date_start = "2015-01-01", date_end = "2016-01-01", db_config) {
    source('functions_common/db_query.R')
    debug_mode <- FALSE
    query <- paste0("SELECT `data_table_name`,`events_table_name`,`template` 
                       FROM `1_cast_park_table_dic` WHERE `wp_code`='",wp_code,
                    "' ORDER BY `id` DESC LIMIT 1")
    rs<-db_query(query=query,  db_config=db_config)
    
    query <- paste0("SELECT * 
                       FROM ",rs$data$data_table_name," 
                      WHERE date_time BETWEEN '",date_start,"' AND '",date_end,"'
                        AND ld_id = ",ld_id,";")

    rs<-db_query(query=query, db_config=db_config)

    rs$data$date_time <- as.POSIXct(rs$data$date_time,tz="UTC",origin="1970-01-01")
    
    return(rs$data)
}