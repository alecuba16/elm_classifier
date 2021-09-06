get_old_predictions <- function( folder = 'C:\\Users\\Usuari\\OneDrive\\1 SmartIve\\Acciona Reportes Parques\\Escambrons',
                     file_list = NULL, omit_last_month = TRUE) {
    idactivo_dic <- c("Escambrons"='ESCAMB',"Moncayuelo"="MONCAY","Izco"="IZCO")
    if(!exists('force_threshold')) force_threshold <- FALSE
    if(!exists('new_threshold')) new_threshold <- 0.5
    if(is.null(file_list)) {
        file_list <- list.files(path = folder, pattern = "[0-9].csv$|d.csv$")
        file_list <- grep("^2016", file_list, value = TRUE)
    }
    date_time <- substr(file_list,start = 1,stop = 10)
    date_time <- as.Date(date_time)
    if(omit_last_month) {
        last_month <- max(date_time)
        file_list <- file_list[!(date_time %in% last_month)]
    }
    
    predictions <- lapply(paste0(folder,'/',file_list), FUN = read.csv)
    threshold <- 0.5
    # Parche para archivos de enero y febrero que no tienen las columnas de "trheshold" y "prediction"
    predictions <- lapply(predictions, FUN = function(x) {
        if(ncol(x) < 10) x <- data.frame(x, threshold, prediction = ifelse(x$prob_fault >= threshold,1,0))
        x
    })
    predictions <- do.call(rbind,predictions)
    predictions$X <- NULL
    splited_path <- strsplit(folder,split = "[\\]")[[1]]
    predictions$idactivo <- factor(idactivo_dic[splited_path[length(splited_path)]])
    predictions <- dplyr::select(predictions, idactivo, everything())
    # Fix problem with different format dates
    temp_date <- as.character(predictions$date_time)
    predictions$date_time <- as.Date(predictions$date_time, format = "%Y-%m-%d")
    id_bad_date <- grep("/",temp_date)
    predictions$date_time[id_bad_date] <- as.Date(temp_date[id_bad_date], format = "%d/%m/%Y")
    # Parche threshold igual a 0.5
    if(force_threshold) {
        predictions <- dplyr::mutate(predictions, threshold = new_threshold, prediction = ifelse(prob_fault >= new_threshold,1,0))
    }
    predictions
}
