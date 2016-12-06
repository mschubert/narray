#' A multidimensional \code{which} function
#'
#' @param x     N-dimensional logical array
#' @param drop  Return a vector if called on a vector
#' @return      A matrix with indices where \code{A == TRUE}
#' @export
which = function(x, drop=TRUE) {
    if (!drop)
        x = as.array(x)

    re = base::which(x, arr.ind=TRUE)
    colnames(re) = names(dimnames(x))
    re
}
