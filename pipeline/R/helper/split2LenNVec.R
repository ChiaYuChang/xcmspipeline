split2LenNVec <- function(x, n) {
    return(split(x, nLenVec(x, n)))
}

nLenVec <- function(x, n) {
    nN <- floor(length(x) / n)
    c(
        rep(1:nN, each = n),
        rep(nN + 1, each = length(x) - n * nN)
    )
}
