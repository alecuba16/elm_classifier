# Return a dataframe with the numeric value of the difftime and their pretty units.
pretty_difftime <- function(x, toprint = T, unit = ifelse(is.difftime(x), units(x), "mins")) {
    source('functions_cast/pretty.unit.R')
    xd <- as.difftime(x, units = unit) # To be sure it is a difftime object
    unit <- tolower(unit)
    nr <- length(xd)
    result <- data.frame(length = numeric(nr), unit = character(nr), 
                         toprint = character(nr),
                         stringsAsFactors = F)
    convert <- ifelse(grepl("min",unit), 60, 
                      ifelse(grepl("hour",unit),60*60,
                             ifelse(grepl("day",unit), 24*60*60, 1)))

    for(i in 1:nr) {
        if(is.na(xd[i])) {
            result$length[i] <- NA
            result$unit[i] <- NA
            result$toprint[i] <- NA
            next()
        }
        xs <- as.numeric(xd[i])*convert
        pu <- pretty.unit(xs)
        pu <- ifelse(pu == "month" | pu == "year", "weeks", paste0(pu,"s"))
        units(xd[i]) <- pu
        result$length[i] <- as.numeric(xd[i])
        result$unit[i] <- pu
        result$toprint[i] <- paste(format(as.numeric(xd[i]), digits = 2),pu)
    }
    if(toprint)
        return(result$toprint)
    return(result)
}
