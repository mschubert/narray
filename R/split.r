#' Splits and array along a given axis, either totally or only subsets
#'
#' @param X        An array that should be split
#' @param along    Along which axis to split; use -1 for highest dimension
#' @param subsets  Whether to split each element or keep some together
#' @param drop     Remove unused dimensions after mapping
#'    default: drop if all resulting arrays have same number of dimensions
#' @return         A list of arrays that combined make up the input array
#' @export
split = function(X, along, subsets=c(1:dim(X)[along]), drop=NULL) {
    if (!is.array(X) && !is.vector(X) && !is.data.frame(X))
        stop("X needs to be either vector, array or data.frame")
    if (along < 0)
        along = length(dim(X)) - along + 1

    if (any(is.na(subsets)))
        warning("'subsets' contains NA, dropping those values")

    usubsets = stats::na.omit(unique(subsets))
    lus = length(usubsets)
    idxList = base::rep(list(base::rep(list(TRUE), length(dim(X)))), lus)

    for (i in 1:lus) {
        cur = subsets==usubsets[i]
        cur[is.na(cur)] = FALSE
        idxList[[i]][[along]] = cur
    }

    if (length(usubsets)!=dim(X)[along] || !is.numeric(subsets)) {
        lnames = usubsets
        if (is.null(drop))
            drop = FALSE
    } else {
        lnames = base::dimnames(X)[[along]]
        if (is.null(drop))
            drop = TRUE
    }

    subs = function(ll) subset(X, ll, drop=drop)
    stats::setNames(lapply(idxList, subs), lnames)
}
