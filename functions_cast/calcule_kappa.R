calcule_kappa <- function(tbl) {
    total <- sum(tbl[c("tp", "fp", "fn", "tn")], na.rm = T) # Only tp, fp, fn and tn are considered. Others are ignored.
    acc <- sum(tbl['tp'],tbl['tn'],na.rm=T)/total
    ma <- (sum(tbl['tp'],tbl['fp'],na.rm=T)*sum(tbl['tp'],tbl['fn'],na.rm=T)) / total
    # mb <- (sum(tbl['tn'],tbl['fp'],na.rm=T)*sum(tbl['tn'],tbl['fn'],na.rm=T)) / total
    mb <- prod(sum(tbl['tn'],tbl['fp'],na.rm=T),sum(tbl['tn'],tbl['fn'],na.rm=T)) / total
    pe <- (ma + mb) / total
    k <- (acc- pe) / (1 - pe)
    k
}
