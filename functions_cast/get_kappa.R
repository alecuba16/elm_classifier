get_kappa <- function(cm) {
    # Check type and levels
    if(class(cm) != "factor"){
        warning("\nCM argument is not a factor. It will be coarsed to a factor asuming these levels: c('tp', 'fp', 'fn', 'tn')")
        cm <- factor(cm, levels = c("tp", "fp", "fn", "tn"))
    }
    # cm <- as.factor(cm)
    # levels(cm) <- c("tp", "fp", "fn", "tn")
    
    tbl <- table(cm)
    total <- sum(tbl[c("tp", "fp", "fn", "tn")]) # Only tp, fp, fn and tn are considered. Others are ignored.
    acc <- (tbl['tp'] + tbl['tn']) / total
    
    ma <- ((tbl['tp'] + tbl['fp'])*(tbl['tp']+tbl['fn'])) / total
    mb <- ((tbl['tn'] + tbl['fp'])*(tbl['tn']+tbl['fn'])) / total
    pe <- (ma + mb) / total
    
    k <- (acc- pe) / (1 - pe)
    return(as.numeric(k))
}