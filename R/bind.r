#' Binds arrays together disregarding names
#'
#' @param ...    N-dimensional arrays, or a list thereof
#' @param along  Along which axis to bind them together (default: new axis)
#' @return       A joined array
#' @export
bind = function(..., along=length(dim(arrayList[[1]]))+1) {
    arrayList = list(...)
    if (length(arrayList) == 1 && is.list(arrayList[[1]]))
        arrayList = arrayList[[1]]

    arrayList = vectors_to_row_or_col(arrayList, along=along)

    ndim = dim(arrayList[[1]])
    ndim[along] = sum(sapply(arrayList, function(a) dim(a)[along]))

    dimNames = dimnames(arrayList[[1]])
    names_along = unlist(lapply(arrayList, dimnames, along=along))
    if (length(names_along) != ndim[along])
        dimNames[along] = list(NULL)
    else
        dimNames[[along]] = names_along

    offset = 0
    index = base::rep(list(TRUE), length(ndim))
    result = array(NA, dim=ndim, dimnames=dimNames)

    for (i in seq_along(arrayList)) {
        cur = arrayList[[i]]
        index[[along]] = offset + seq_len(dim(cur)[along])
        result = do.call("[<-", c(list(result), index, list(cur)))
        offset = offset + length(index[[along]])
    }

    if (dim(result)[along] == length(arrayList) && !is.null(names(arrayList)))
        dimnames(result)[[along]] = names(arrayList)

    restore_null_dimnames(result)
}
