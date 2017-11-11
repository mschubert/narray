#' Subsets an array using a list with indices or names
#'
#' @param X      The array to subset
#' @param index  A list of vectors to use for subsetting, or vector if along is given
#' @param along  Along which dimension to subset if index is a vector; default
#'               is last dimension; argument is ignored if X is a vector
#' @param drop   Remove unused dimensions after mapping; default: TRUE
#' @return       The subset of the array
#' @export
subset = function(X, index, along=NULL, drop=FALSE) {
    if (!is.list(index)) {
        # ignore along for vectors
        if (is.atomic(X) && is.vector(X))
            along = 1

        # this is required because as.array() will fail on dplyr:df
        if (is.data.frame(X))
            ndim_X = length(dim(X))
        else
            ndim_X = length(dim(as.array(X)))

        # create a subsetting list that covers the whole array first,
        # then set the dimension we are working on to what is requested
        tmp = base::rep(list(TRUE), ndim_X)

        # by default, subset the last dimension
        if (is.null(along))
            along = ndim_X
        tmp[[along]] = index
        index = tmp
    }

    if (any(is.na(unlist(index))))
        stop("trying to subset with NA in index")

    do.call(function(...) `[`(X, ..., drop=drop), index)
}
