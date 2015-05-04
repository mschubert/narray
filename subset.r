#' Subsets an array using a list with indices or names
#'
#' @param X      The array to subset
#' @param index  A list of vector to use for subsetting
#' @param along  Along which dimension to subset if index is a vector; default is last dimension
#' @return       The subset of the array
subset = function(X, index, along=NULL, drop=FALSE) {
    if (is.list(index))
        abind::asub(X, index, drop=drop)
    else {
        if (is.null(along))
            along = length(dim(X))
        plyr::take(X, along=along, indices=index, drop=drop)
    }
}
