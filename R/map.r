#' Apply function that preserves order of dimensions
#'
#' @param X        An n-dimensional array
#' @param along    Along which axis to apply the function
#' @param FUN      A function that maps a vector to the same length or a scalar
#' @param drop     Remove unused dimensions after mapping; default: TRUE
#' @return         An array where \code{FUN} has been applied
map_simple = function(X, along, FUN, drop=TRUE) {
    if (is.vector(X) || length(dim(X))==1)
        return(FUN(X))

    preserveAxes = c(1:length(dim(X)))[-along]
    Y = as.array(apply(X, preserveAxes, FUN))
    if (length(dim(Y)) < length(dim(X)))
        Y = array(Y, dim=c(1, dim(Y)), dimnames=c(list(NULL), dimnames(Y)))

    Y = aperm(Y, base::match(seq_along(dim(Y)), c(along, preserveAxes)))
    if (drop)
        drop(Y)
    else
        Y
}

#' Maps a function along an array preserving its structure
#'
#' @param X        An n-dimensional array
#' @param along    Along which axis to apply the function
#' @param FUN      A function that maps a vector to the same length or a scalar
#' @param subsets  Whether to apply \code{FUN} along the whole axis or subsets thereof
#' @param drop     Remove unused dimensions after mapping; default: TRUE
#' @return         An array where \code{FUN} has been applied
#' @export
map = function(X, along, FUN, subsets=base::rep(1,dim(X)[along]), drop=TRUE) {
#    .check$all(X, along, subsets, x.to.array=TRUE)

    subsets = as.factor(subsets)
    lsubsets = as.character(unique(subsets)) # levels(subsets) changes order!
    nsubsets = length(lsubsets)

    # create a list to index X with each subset
    subsetIndices = base::rep(list(base::rep(list(TRUE), length(dim(X)))), nsubsets)
    for (i in 1:nsubsets)
        subsetIndices[[i]][[along]] = (subsets==lsubsets[i])

    # for each subset, call mymap
    resultList = lapply(subsetIndices, function(f)
        map_simple(subset(X, f), along, FUN, drop=FALSE))

    # assemble results together
    Y = bind(resultList, along=along)
    if (dim(Y)[along] == nsubsets)
        base::dimnames(Y)[[along]] = lsubsets
    else if (dim(Y)[along] == dim(X)[along])
        base::dimnames(Y)[[along]] = base::dimnames(X)[[along]]

    if (drop)
        drop(Y)
    else
        Y
}
