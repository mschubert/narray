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
