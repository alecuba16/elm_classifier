format_old_predictions <- function() {
    library(dplyr)
    library(lubridate)
    source("functions_cast/get_old_predictions.R")
    source("functions_cast/check_acciona_rules.R")
    
    events <- data.frame(idactivo = c('ESCAMB', 'MONCAY', 'MONCAY', 'MONCAY','IZCO', 'IZCO','IZCO', 'IZCO'),
                         ld_id = c(128, 159, 142, 162, 178, 195, 209,180),
                         fault = c('gbox', 'gbox', 'gbox', 'gen','gen', 'gbox', 'gen', 'gen'),
                         date_time = c('2016-01-18', '2016-01-05','2016-06-28', '2016-08-09','2016-03-19', '2016-06-02', '2016-09-08', '2016-09-11'),
                         alarm = 1)
    
    folders <- c("Escambrons","Moncayuelo","Izco")
    folders <- paste0('C:\\Users\\Usuari\\OneDrive\\1 SmartIve\\Acciona Reportes Parques\\',folders)
    
    pred <- lapply(folders, get_old_predictions, omit_last_month = FALSE)
    pred <- do.call(rbind,pred)
    
    pred$month <- month(pred$date_time)
    pred <- filter(pred, date_time >= '2016-01-01' & date_time < '2017-01-01')
    
    # Parche para corregir error de "anticipation" mal etiquetada de enero a mayo 2016
    pred$anticipation[ pred$anticipation < 30 ] <- 30 
    
    pred$idactivo <- as.character(pred$idactivo)

    pred$fault <- as.character(pred$fault)
    pred$fault[grep(pattern = "^gbox", x = pred$fault)] <- 'gbox'
    pred$fault[grep(pattern = "^gen", x = pred$fault)] <- 'gen'
    pred <- dplyr::arrange(pred, idactivo, fault, anticipation, ld_id, date_time)
    
    predx <- merge(pred, events, by = c('idactivo','fault','ld_id','date_time'), all.x = TRUE, all.y = TRUE, sort = F)
    predx$alarm[is.na(predx$alarm)] <- 0
    predx <- dplyr::arrange(predx, idactivo, fault, anticipation, ld_id, date_time)
    
    head(predx)
    
    get_valoracion <- function(x, ant=0, ant_offset = 30, mar=15, ant_max=30*4){
        ant2 <- ant
        if(ant >= ant_offset) # Parche: se espera que los valores vengan ya desplazados 30 días. Por eso el offset a 30. 
            ant2 <- ant - ant_offset
        date_alarm <- x$date_time[x$alarm==1]
        x$fallos_cp <- numeric(nrow(x))
        x$fallos_cp[ ( x$date_time <= (date_alarm - period(ant2, units = "day")) ) & ( x$date_time >= (date_alarm - period(ant2 + mar, units = "day")) )] <- 1
        
        x$fallos_lp <- numeric(nrow(x))
        x$fallos_lp[ x$date_time < ceiling_date(date_alarm,"month") & x$date_time >= (date_alarm - period(ant_max, units = "day"))] <- 1
        
        x$valoracion <- ifelse( (x$fallos_cp | x$fallos_lp) & x$prediction == 1, 'tp',
                                ifelse( x$fallos_cp & x$prediction == 0, 'fn',
                                        ifelse( (!x$fallos_cp & !x$fallos_lp) & x$prediction == 1, 'fp', 'tn' ) ) )
        # x$anticipation <- ant
        x
    }
    # x <- filter(predx, ld_id == 128, fault == 'gbox')
    # vx <- get_valoracion(x, ant = 0, mar = 15, ant_max = 30*4)
    # calculate_kappa(table(vx$valoracion))
    
    lpredx <- split(predx, paste0(predx$fault,'_',predx$ld_id))
    lpredx <- lapply(lpredx, get_valoracion)
    
    predx <- do.call(rbind,lpredx)
    
    accx <- dplyr::select(predx, idactivo, turbina = ld_code, ld_id, componente = fault, fecha = date_time, mes = month, anticipation, prob_fault:valoracion)
    
    accx

}
