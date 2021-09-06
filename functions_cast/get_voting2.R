# get_votingr: Get voting results. Return the voting variable and the means for
# values according the alarm's categories (false or true)
get_votingr <- function(wtdata) {
    # Calculate means (TODO: 66% wtdata)
    n <- dim(wtdata)[1]
    m <- dim(wtdata)[2]
    
    means_n <- numeric(m)
    means_p <- numeric(m)
    #near_xp <- matrix(nrow = n, ncol = (m-1))
    voting <- matrix(nrow = n, ncol = 1)
    
    for( i in 1:(m-1) ) {
        x <- wtdata[,i] # BY the moment I use the full data, but I shouldn't use it, instead I should use the trn dataset
        if( is.numeric(x) ) {
            by_mean <- by(x, wtdata[,m], mean, na.rm = TRUE)
            means_n[i] <- by_mean[1]
            means_p[i] <- by_mean[2]
            #means_n[i] <- mean(x[wtdata[,m] == "f"], na.rm = TRUE)
            #means_p[i] <- mean(x[wtdata[,m] == "t"], na.rm = TRUE)
        }
    }
    rm(x)
    
    # Calculate distances
    Dn <- abs( wtdata[,1:(m-1)] - matrix( rep(means_n[1:(m-1)],n), nrow = n, ncol = (m-1), byrow = TRUE ) )
    Dp <- abs( wtdata[,1:(m-1)] - matrix( rep(means_p[1:(m-1)],n), nrow = n, ncol = (m-1), byrow = TRUE ) )
    
    # Calculate proximity and count voting
    near_xp <- Dp < Dn
    voting <- rowSums(near_xp,na.rm = TRUE)
    results <- list(voting = voting, means_n = means_n, means_p = means_p)
    return(results)
}

# get_voting2: calcules the voting variable using the means (means_n, means_p) 
# cumputed before training. Used which new data (wtdata).
get_voting2 <- function(wtdata, means_n, means_p) {
    # Calculate means (TODO: 66% wtdata)
    n <- dim(wtdata)[1]
    m <- dim(wtdata)[2]
    
    #near_to <- matrix(nrow = n, ncol = m)
    voting <- matrix(nrow = n, ncol = 1)
    
    # Calculate distances
    Dn <- abs( wtdata[,1:(m-1)] - matrix( rep(means_n[1:(m-1)],n), nrow = n, ncol = (m-1), byrow = TRUE ) )
    Dp <- abs( wtdata[,1:(m-1)] - matrix( rep(means_p[1:(m-1)],n), nrow = n, ncol = (m-1), byrow = TRUE ) )
    
    # Calculate proximity and count voting
    near_xp <- Dp < Dn
    voting <- rowSums(near_xp,na.rm = TRUE)

    return(voting)
}
