#' Function to discard subsets of an array (NA or drop)
#'
#' @param X        An n-dimensional array
#' @param along    Along which axis to apply \code{FUN}
#' @param FUN      Function to apply, needs to return \code{TRUE} (keep) or \code{FALSE}
#' @param subsets  Subsets that should be used when applying \code{FUN}
#' @param na.rm    Whether to omit columns and rows with \code{NA}s
#' @return         An array where filtered values are \code{NA} or dropped
#' @export
filter = function(X, along, FUN, subsets=base::rep(1,dim(X)[along]), na.rm=FALSE) {
#    .check$all(X, along, subsets)

    X = as.array(X)
    # apply the function to get a subset mask
    mask = as.array(map(X, along, function(x) FUN(x), subsets, drop=FALSE))
    if (mode(mask) != 'logical' || dim(mask)[1] != length(unique(subsets)))
        stop("FUN needs to return a single logical value")

    for (mcol in seq_along(ncol(mask)))
        for (msub in rownames(mask))
            if (!mask[msub, mcol])
                X[subsets==msub, mcol] = NA #FIXME: work for matrices as well

    if (na.rm)
        t(stats::na.omit(t(stats::na.omit(X))))
    else
        X
}
