#' Converts a list of character vectors to a logical matrix
#'
#' @param x      A list of character vectors
#' @param along  Which axis to spread mask on
#' @param na_rm  Remove values that were translated to NAs
#' @return       A logical occurrence matrix
#' @export
mask = function(x, along=2, na_rm=FALSE) {
    if (is.factor(x))
        x = as.character(x)

    list2logic = function(xi) {
        re = stats::setNames(base::rep(TRUE, length(xi)), xi)
        re[!is.na(xi)] #TODO: keep as NAs if na_rm=F, otherwise use as "not x"
    }

    vectorList = lapply(x, list2logic)
    stacked = stack(vectorList, keep_empty=TRUE, fill=FALSE)

    if (along == 1)
        stacked
    else
        t(stacked)
}
