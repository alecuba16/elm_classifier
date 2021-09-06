clean_null_code <- function(wtdata, null_code) {
    xxl <- as.list(wtdata)
    xxl <- lapply( xxl, FUN = function(x) { x[x==null_code] <- NA; x})
    wtdata <- as.data.frame(xxl)
    wtdata
}