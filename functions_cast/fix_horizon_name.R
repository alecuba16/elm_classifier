fix_horizon_name <- function(results, horizons) {
    change_name <- function(x, h) {names(x) <- as.character(h); x}
    lapply(results, change_name, horizons)
    
}

# fix_horizon_name <- function(results, horizons) {
#     names(results[[1]]) <- as.character(horizons)
#     results
# }