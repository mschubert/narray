.s = import('./subset')

#' Apply function that preserves order of dimensions
#'
#' @param X        An n-dimensional array
#' @param along    Along which axis to apply the function
#' @param FUN      A function that maps a vector to the same length or a scalar
map_simple = function(X, along, FUN) { #TODO: replace this by alply?
    if (is.vector(X) || length(dim(X))==1)
        return(FUN(X))

    preserveAxes = c(1:length(dim(X)))[-along]
    Y = apply(X, preserveAxes, FUN)
    if (is.vector(Y)) {
        if (along == 1) {
            newdim = c(1, length(Y))
            newdimnames = list(NULL, names(Y))
        } else {
            newdim = c(length(Y), 1)
            newdimnames = list(names(Y), NULL)
        }
        array(Y, dim=newdim, dimnames=newdimnames)
    } else {
        if (length(dim(Y)) < length(dim(X)))
            Y
        else
            aperm(Y, c(along, preserveAxes))
    }
}

#' Maps a function along an array preserving its structure
#'
#' @param X        An n-dimensional array
#' @param along    Along which axis to apply the function
#' @param FUN      A function that maps a vector to the same length or a scalar
#' @param subsets  Whether to apply \code{FUN} along the whole axis or subsets thereof
#' @param drop     TBD
#' @return         An array where \code{FUN} has been applied
map = function(X, along, FUN, subsets=rep(1,dim(X)[along]), drop=TRUE) {
#    .check$all(X, along, subsets, x.to.array=TRUE)

    subsets = as.factor(subsets)
    lsubsets = as.character(unique(subsets)) # levels(subsets) changes order!
    nsubsets = length(lsubsets)

    # create a list to index X with each subset
    subsetIndices = rep(list(rep(list(TRUE), length(dim(X)))), nsubsets)
    for (i in 1:nsubsets)
        subsetIndices[[i]][[along]] = (subsets==lsubsets[i])

    # for each subset, call mymap
    resultList = lapply(subsetIndices, function(f)
        map_simple(.s$subset(X, f), along, FUN))
#    resultList = lapply(subsetIndices, function(x) alply(subset(X, f), along, FUN)) FIXME:

    # assemble results together
    Y = do.call(function(...) abind::abind(..., along=along), resultList)
    if (dim(Y)[along] == nsubsets)
        base::dimnames(Y)[[along]] = lsubsets
    else if (dim(Y)[along] == dim(X)[along])
        base::dimnames(Y)[[along]] = base::dimnames(X)[[along]]
    drop(Y)
}

if (is.null(module_name())) {
    D = structure(c(1L, 2L, 3L, 4L, NA, NA, 1L, 2L, 3L, 4L, 6L, 5L),
                  .Dim = c(2L, 3L, 2L), .Dimnames = list(c("a", "b"),
                  c("x", "y", "z"), c("m", "n")))

    X = map(D, along=1, function(x) sum(x, na.rm=TRUE))
    #   m  n
    # x 3  3
    # y 7  7
    # z 0 11

    Xref = structure(c(3L, 7L, 0L, 3L, 7L, 11L), .Dim = c(3L, 2L),
                     .Dimnames = list(c("x", "y", "z"), c("m", "n")))
    testthat::expect_equal(X, Xref)
}
