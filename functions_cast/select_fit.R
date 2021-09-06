select_fit <- function(metric, sma, fit_list, forced_model = NULL) {
    if(is.null(forced_model))
        if( metric == "ROC" | metric == "Acc" | metric == "Kappa" ) {
            id_best <- which.max(sma[,as.character(metric)])
        } else {
            id_best <- which.min(sma[,as.character(metric)])
        }
    else {
        id_best <- which(sma$fitmodel == forced_model)
        if(length(id_best) < 1 )
            stop(forced_model, "no found. It should be one of these: ", paste(sma$fitmodel, collapse = "; "))
    }
    result <- list(horizon = sma$horizon[id_best],
                   fitname = sma$fitmodel[id_best],
                   fit = fit_list[[id_best]],
                   sma = sma[id_best,])
    # Check if right fit was selected
    if(result$fit$method != result$fitname)
        warning("Wrong fit model selected!!!")
    
    return(result)
}