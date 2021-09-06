get_input_ranking <- function(path, horizon, model_name) {
    library(dplyr)
    source('functions_common/extract_fit.R')
    
    ## 1. Load trained models
    con <- gzfile(path)
    load <- try( load(con) )
    close(con)
    
    f_list <- results[[1]][[as.character(horizon)]]$fit_list
    model <- extract_fit(f_list, model_name, return_id = FALSE)
    rank_imp <- varImp(model)$importance
    rank_imp$names <- rownames(rank_imp)
    if(model_name == "nb") {
        rank_imp <- arrange(rank_imp, desc(t))
    }else {
        rank_imp <- arrange(rank_imp, desc(Overall))
    }
    # ntop <- min(4,nrow(rank_imp))
    # top4 <- rank_imp$names[1:ntop]
    return(rank_imp)
}