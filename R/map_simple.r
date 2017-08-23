#' Apply function that preserves order of dimensions
#'
#' @param X        An n-dimensional array
#' @param along    Along which axis to apply the function
#' @param FUN      A function that maps a vector to the same length or a scalar
#' @param drop     Remove unused dimensions after mapping; default: TRUE
#' @param ...      Arguments passed to the function
#' @return         An array where \code{FUN} has been applied
map_simple = function(X, along, FUN, drop=TRUE, ...) {
    if (is.vector(X) || length(dim(X))==1)
        return(FUN(X, ...))

    preserveAxes = c(1:length(dim(X)))[-along]
    Y = as.array(apply(X, preserveAxes, FUN, ...))
    if (length(dim(Y)) < length(dim(X)))
        Y = array(Y, dim=c(1, dim(Y)), dimnames=c(list(NULL), dimnames(Y)))

    Y = aperm(Y, base::match(seq_along(dim(Y)), c(along, preserveAxes)))
    if (drop)
        drop(Y)
    else
        Y
}
