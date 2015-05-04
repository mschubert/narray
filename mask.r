.s = import('./stack')

#' Converts a list of character vectors to a logical matrix
#'
#' @param x  A list of character vectors
#' @return   A logical occurrence matrix
mask = function(x) {
    if (is.factor(x))
        x = as.character(x)

    vectorList = lapply(x, function(xi) setNames(rep(TRUE, length(xi)), xi))
    t(.s$stack(vectorList, fill=FALSE))
}
