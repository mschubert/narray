#' Converts a list of character vectors to a logical matrix
#'
#' @param x      A list of character vectors
#' @param along  Which axis to spread mask on
#' @return       A logical occurrence matrix
#' @export
mask = function(x, along=2) {
    if (is.factor(x))
        x = as.character(x)

    list2logic = function(xi) stats::setNames(base::rep(TRUE, length(xi)), xi)

    vectorList = lapply(x, list2logic)
    stacked = stack(vectorList, fill=FALSE)

    if (along == 1)
        stacked
    else
        t(stacked)
}
