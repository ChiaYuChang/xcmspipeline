#' Create functions that coerce objects of given type. If any elements in the vector
#' can not be properly converted, throw error.
#' @param x a vector
#' @param fun a function that coerce objects
#' @return a function coerce objects of given type
as_type_fun <- function(x, fun) {
        type <- str_remove(substitute(fun), "as.")
        f <- function(x) {
                naPosBefore <- which(is.na(x))
                xConverted  <- suppressWarnings(fun(x))
                naPosAfter  <- which(is.na(xConverted))
                converFailedPos <- setdiff(naPosAfter, naPosBefore)
                if (length(converFailedPos) > 0) {
                        stop(sprintf("The value(s), %s, could not be converted to type %s correctly.", 
                                     paste(x[converFailedPos], collapse = ", "), type))
                }
                return(xConverted)
        }
        return(f)
}

as_numeric    <- as_type_fun(x, as.numeric)
as_integer    <- as_type_fun(x, as.integer)
as_character  <- as_type_fun(x, as.character)
as_logical    <- function(x) {
        x <- switch (x,
                "1" = TRUE,
                "0" = FALSE,
                x
        )
        return(as_type_fun(x, as.logical)(x))
}