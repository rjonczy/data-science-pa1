corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    files <- list.files(directory, full.names = TRUE)
    
    correlations <- c()
    
    for (f in files) {
        d <- read.csv(f)
        
        if(sum(complete.cases(d)) > threshold) {            
            correlations <- c(correlations, cor(d$sulfate, d$nitrate, use = "complete.obs"))
        }
        
    }
    correlations
    
}