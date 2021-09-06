pretty.unit <- function (x) 
{
    if (x > 3600 * 24 * 365) 
        return("year")
    if (x > 3600 * 24 * 30) 
        return("month")
    if (x > 3600 * 24) 
        return("day")
    if (x > 3600) 
        return("hour")
    if (x > 60) 
        return("min")
    else return("sec")
}