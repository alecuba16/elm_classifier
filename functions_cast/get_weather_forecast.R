#' get_weather_forecast: it takes the reported weather from historical data as
#' weather forecast. If the ld_id variable is NULL, average and maximum 
#' information from all the wind farm specified by wp_code will be used. Data is
#' grouped daily.
#'
#' @param wp_code Park code used to know the table to get the weather data
#' @param ld_id If it is specified, only the WT data will be used. If NULL all 
#' WT data will be used.
#' @param date_time_array Arrary of the dates of the day before of the forecast. 
#' Like the current day.Forecast will be taken fron the next day till the horizon 
#' maximum.
#' @param horizon_max The maximum horizon of the weather forecast.
#' @param host URL that specifies where is the server of the DB. Data will be 
#' taken always from historical DB.
#'
#' @return
#' @export
#'
#' @examples
get_weather_forecast <- function(wp_code = "rob", ld_id = NULL, date_time_array = Sys.Date(), 
                                 horizon_max = 28, force_forecast = FALSE,db_config) {
    source("functions_common/db_query.R")
    source("functions_common/db_check_dic.R")
    park_table_dic <- "1_cast_park_table_dic"

    data_table_name <- db_check_dic(park_table_dic,"data_table_name","wp_code", 
                                 wp_code, db_config = db_config)$data[1]
    wind_avg_name <- db_check_dic(park_table_dic,"wind_name","wp_code", 
                              wp_code, db_config = db_config)$data[1]
    if(!is.null(ld_id)) {
        ld_id_condition <- paste0(" AND ld_id = ",ld_id)
    } else {
        ld_id_condition <- ""
    }
    wf <- data.frame(date_time = character(), wind_avg = numeric(), 
                     wind_max = numeric(), horizon = numeric(),
                     stringsAsFactors = FALSE)
    
    for(date_time in date_time_array) {
        d1 <- as.Date(date_time, origin = "1970-01-01") + 1
        d2 <- as.Date(date_time, origin = "1970-01-01") + horizon_max
        in_dates0 <- as.Date(d1:d2, origin = "1970-01-01")
        query <- paste0(
            "SELECT DATE(date_time) AS 'date_time', 
                    AVG(",wind_avg_name,") AS 'wind_avg',
                    MAX(",wind_avg_name,") AS 'wind_max' 
               FROM ",data_table_name,
            " WHERE date_time BETWEEN '",d1,"' AND '",d2," 23:59:59'", ld_id_condition,
            " GROUP BY DATE(date_time)
              ORDER BY 1"
        )
        rs <- db_query(query, db_config=db_config)
        rs$data$date_time <- as.Date(rs$data$date_time, origin = "1970-01-01") # To be sure it is Date.

        counter <- 1
        while( nrow(rs$data) < horizon_max && force_forecast && counter <= 2) {
            in_dates <- as.Date(setdiff(in_dates0, as.Date(rs$data$date_time, origin = "1970-01-01")), origin = "1970-01-01")
            in_dates <- as.Date(in_dates - 365*counter, origin = "1970-01-01")

            query <- paste0(
                "SELECT DATE(date_time) AS 'date_time', 
                        AVG(",wind_avg_name,") AS 'wind_avg',
                        MAX(",wind_avg_name,") AS 'wind_max' 
                   FROM ",data_table_name,
                " WHERE DATE(date_time) IN ('",paste(in_dates, collapse = "','"),"')", ld_id_condition,
                " GROUP BY DATE(date_time)
              ORDER BY 1"
            )
            rs_pass <- db_query(query, db_config=db_config)
            
            rs_pass$data$date_time <- as.Date(rs_pass$data$date_time, origin = "1970-01-01") + 365*counter
            rs$data <- rbind(rs$data, rs_pass$data)
            counter <- counter + 1
        }
        h <- as.numeric( as.Date(rs$data$date_time, origin = "1970-01-01") - as.Date(date_time, origin = "1970-01-01"))
        wf <- rbind( wf,data.frame( rs$data, horizon = h ) )
        
        
    }
    
    # ------------------------------------------------------------------------ #
    # Check limits for wind forecast
    # wind_max
    wf$wind_max <- ifelse(wf$wind_max < 1, 1, wf$wind_max) # Low limit
    wf$wind_max <- ifelse(wf$wind_max > 45, 45, wf$wind_max) # High limit
    # wind_avg
    wf$wind_avg <- ifelse(wf$wind_avg < 0, 0, wf$wind_avg) # Low limit
    wf$wind_avg <- ifelse(wf$wind_avg > 20, 20, wf$wind_avg) # High limit
    # ------------------------------------------------------------------------ #
    # Fill gaps
    fill_NAs <- function(x) { x[is.na(x)] <- mean(x, na.rm = TRUE); x }
    wf$wind_avg <- fill_NAs(wf$wind_avg)
    wf$wind_max <- fill_NAs(wf$wind_max)
    # ------------------------------------------------------------------------ #
    
    return(wf)
}

# First version reading data from csv
# get_weather_forecast <- function(wp_code = "rob_v90", date_time = Sys.Date(), horizon_max = 28) {
#     wf2 <- read.csv(file = "15-12-17_weather_forecast_cova_de_serpe.csv")
#     wf2$horizon <- 1:nrow(wf2)
#     return(wf2)
# }