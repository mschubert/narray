#' base::dim, but returning 1 for vector
#'
#' @param x  Object to get dimensions on
#' @export
dim = function(x) {
    if (is.vector(x))
        length(x)
    else
        base::dim(x)
}
