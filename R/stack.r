#' Stacks arrays while respecting names in each dimension
#'
#' @param ...         N-dimensional arrays, or a list thereof
#' @param along       Which axis arrays should be stacked on (default: new axis)
#' @param fill        Value for unknown values (default: \code{NA})
#' @param drop        Drop unused dimensions (default: FALSE)
#' @param keep_empty  Keep empty elements when stacking (default: FALSE)
#' @param allow_overwrite  Overwrite values if more arrays share same key
#' @param fail_if_empty    Stop if no arrays left after removing empty elements
#' @return            A stacked array, either n or n+1 dimensional
#' @export
stack = function(..., along=length(dim(arrayList[[1]]))+1, fill=NA, drop=FALSE,
                 keep_empty=FALSE, allow_overwrite=FALSE, fail_if_empty=TRUE) {

    arrayList = list(...)
    if (length(arrayList) == 1 && is.list(arrayList[[1]]))
        arrayList = arrayList[[1]]

    if (!is.list(arrayList))
        stop(paste("arrayList needs to be a list, not a", class(arrayList)))
    length0 = sapply(arrayList, length) == 0
    if (!keep_empty && any(length0)) {
        drop_idx = names(arrayList)[length0]
        if (is.null(drop_idx))
            drop_idx = which(length0)
        arrayList = arrayList[!length0]
    }
    if (length(arrayList) == 0) {
        if (fail_if_empty)
            stop("No element remaining after removing NULL entries")
        else
            return(NULL)
    }

    arrayList = vectors_to_row_or_col(arrayList, along=along)
    result = cpp_stack(arrayList, along, fill, allow_overwrite)
    drop_if(result, drop)
}
