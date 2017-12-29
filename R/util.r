#' If no dimnames, return NULL and not list of NULLs
restore_null_dimnames = function(x) {
    if (all(sapply(dimnames(x), is.null)))
        dimnames(x) = NULL
    x
}

#' Drop unused dims if flag is TRUE
drop_if = function(x, flag) {
    if (flag)
        drop(x)
    else
        x
}

#' Converts vectors in a list to row- or column vectors
#'
#' @param xlist  List of array-like elements and vectors
#' @param along  Along which dimension vectors should be aligned
#' @return       List where vectors are replaced by row- or col vectors (2d)
vectors_to_row_or_col = function(xlist, along) {
	# for vectors: if along=1 row vecs, along=2 col vecs, etc.
    if (all(is.null(unlist(lapply(xlist, base::dim))))) {
        if (along == 1)
            xlist = lapply(seq_along(xlist), function(i) {
                if (is.null(xlist[[i]]))
                    xlist[[i]] = numeric()

                re = t(as.matrix(xlist[[i]]))
                rownames(re) = names(xlist)[i]
                re
            })
        else if (along == 2)
            xlist = lapply(seq_along(xlist), function(i) {
                if (is.null(xlist[[i]]))
                    xlist[[i]] = numeric()

                re = as.matrix(xlist[[i]])
                colnames(re) = names(xlist)[i]
                re
            })
    } else
        xlist
}

#' Operator for array-like logical operations
#'
#' @param a  First vector
#' @param b  Second vector
#' @return   TRUE/FALSE for each element
`%or%` = function(a, b) {
    cmp = function(a,b) if (identical(a, FALSE) ||
                            is.null(a) ||
                            is.na(a) ||
                            is.nan(a) ||
                            length(a) == 0 ||
                            nchar(a) == 0) b else a

    if (is.list(a))
        lapply(1:length(a), function(i) cmp(a[[i]], b[[i]]))
    else if (length(a) > 1) #TODO: does that do what we want?
        mapply(cmp, a, b)
    else
        cmp(a, b)
}
