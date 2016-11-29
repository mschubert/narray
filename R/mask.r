#' Converts a list of character vectors to a logical matrix
#'
#' @param x  A list of character vectors
#' @return   A logical occurrence matrix
#' @export
mask = function(x) {
    if (is.factor(x))
        x = as.character(x)

    vectorList = lapply(x, function(xi) stats::setNames(rep(TRUE, length(xi)), xi))
    t(stack(vectorList, fill=FALSE))
}
