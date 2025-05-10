
# helpers.R: helpers and utility functions --------------------------------


breaks <- function(n) tagList(lapply(1:n, function(x) br()))


format_duration <- function(duration) {
    if(is.null(duration) || is.character(duration)) {
        return("")
    }
    
    if(duration < 60) {
        return(sprintf("%ss", round(duration)))
    } else if(duration < 3600) {
        minutes <- duration %/% 60
        seconds <- round(duration %% 60)
        
        return(sprintf("%sm %ss", minutes, seconds))
    } else {
        hours <- duration %/% 3600
        minutes <- round((duration %% 3600) / 60)
        
        return(sprintf("%sh %sm", hours, minutes))
    }
}


time2mins <- function(times) {
    m <- t(sapply(strsplit(times, ":"), as.numeric))
    m[, 1] * 60 + m[, 2]
}


mins2time <- function(mins) {
    sprintf("%02d:%02d", mins %/% 60, mins %% 60)
}
