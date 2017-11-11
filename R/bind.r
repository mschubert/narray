#' Binds arrays together disregarding names
#'
#' @param arrayList  A list of n-dimensional arrays
#' @param along      Along which axis to bind them together; default: new axis
#' @return           A joined array
#' @export
bind = function(arrayList, along=length(dim(arrayList[[1]]))+1) {
    arrayList = vectors_to_row_or_col(arrayList, along=along)
    re = do.call(function(...) abind::abind(..., along=along), arrayList)

    if (dim(re)[along] == length(arrayList) && !is.null(names(arrayList)))
        dimnames(re)[[along]] = names(arrayList)

    # replace list of NULLs to one NULL to be consistent with base R
    if (all(sapply(dimnames(re), is.null)))
        dimnames(re) = NULL

    re
}
