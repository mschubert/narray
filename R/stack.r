#' Stacks arrays while respecting names in each dimension
#'
#' @param ...         N-dimensional arrays, or a list thereof
#' @param along       Which axis arrays should be stacked on (default: new axis)
#' @param fill        Value for unknown values (default: \code{NA})
#' @param keep_empty  Keep empty elements when stacking (default: FALSE)
#' @param drop        Drop unused dimensions (default: FALSE)
#' @param fail_if_empty  Stop if no arrays left after removing empty elements
#' @return            A stacked array, either n or n+1 dimensional
#' @export
stack = function(..., along=length(dim(arrayList[[1]]))+1, fill=NA,
                 drop=FALSE, keep_empty=FALSE, fail_if_empty=TRUE) {

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

    newAxis = FALSE
    if (along > length(dim(arrayList[[1]])))
        newAxis = TRUE

    # get dimension names; dimNames is set of all elements in list
    dn = dimnames(arrayList)
    dimNames = lapply(1:length(dn[[1]]), function(j) 
        unique(c(unlist(sapply(1:length(dn), function(i) 
            dn[[i]][[j]]
        ))))
    )
    # check if names are valid
    all_names = unlist(dimNames)
    if (any(is.na(all_names)))
        stop("NA found in list dimension names")
    if (any(nchar(all_names) == 0))
        stop("Empty dimension name found in list")

    # track the stacking dimension index if there are no names
    stack_offset = FALSE
    ndim = sapply(dimNames, length)
    if (along <= length(ndim) && ndim[along] == 0) {
        ndim[along] = sum(sapply(arrayList, function(x) dim(x)[along]))
        stack_offset = TRUE
    }
    if (any(ndim == 0))
        stop("Names are required for all dimensions except the one stacked along.
  Use bind() if you want to just bind together arrays without names.")

    # if creating new axis, amend ndim and dimNames
    if (newAxis) {
        dimNames = c(dimNames, list(names(arrayList)))
        ndim = c(ndim, length(arrayList))
    }

    # create an empty result matrix
    result = array(fill, dim=ndim, dimnames=dimNames)

    # fill each result matrix slice with matched values of arrayList
    offset = 0
    for (i in seq_along(arrayList)) {
        dm = dimnames(arrayList[[i]], null_as_integer=TRUE)
        if (stack_offset) {
            dm[[along]] = dm[[along]] + offset
            offset = offset + dim(arrayList[[i]])[along]
        }

        if (newAxis)
            dm[[along]] = i
        else {
            # do not overwrite values unless empty or the same
            slice = do.call("[", c(list(result), dm, drop=FALSE))
            if (!all(slice==fill | is.na(slice) | slice==arrayList[[i]]))
                stop("value aggregation not allowed, stack along new axis+summarize after")
        }

        # assign to the slice if there are any values in it
        result = do.call("[<-", c(list(result), dm, list(arrayList[[i]])))
    }

    drop_if(result, drop)
}
