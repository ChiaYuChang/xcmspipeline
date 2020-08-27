#' Replace each NA in a vector by the most recent non-NA prior to it.
#' @param vec A vector
#' @return An vector in which each NA in the input vector is replaced by the most recent non-NA prior to it.
#' @examples 
#' na_locf(c(1, NA, NA, NA, 2, NA, NA , 3, NA, NA, NA, NA, 4, NA))
na_locf <- function(vec) {
        vec.nNA <- which(!is.na(vec))
        if (length(vec.nNA) > 0) {
                return(rep(vec[vec.nNA], times = c(vec.nNA[-1], length(vec) + 1) - vec.nNA))
        } else {
                return(vec)
        }
}

