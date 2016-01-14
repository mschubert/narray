.s = import_('./subset')

#' Splits and array along a given axis, either totally or only subsets
#'
#' @param X        An array that should be split
#' @param along    Along which axis to split; use -1 for highest dimension
#' @param subsets  Whether to split each element or keep some together
#' @return         A list of arrays that combined make up the input array
split = function(X, along, subsets=c(1:dim(X)[along]), drop=FALSE) {
    if (!is.array(X) && !is.vector(X) && !is.data.frame(X))
        stop("X needs to be either vector, array or data.frame")
    if (along == -1)
        along = length(dim(X))

    usubsets = unique(subsets)
    lus = length(usubsets)
    idxList = rep(list(rep(list(TRUE), length(dim(X)))), lus)

    for (i in 1:lus)
        idxList[[i]][[along]] = subsets==usubsets[i]

    if (length(usubsets)!=dim(X)[along] || !is.numeric(subsets))
        lnames = usubsets
    else
        lnames = base::dimnames(X)[[along]]
    setNames(lapply(idxList, function(ll) .s$subset(X, ll, drop=drop)), lnames)
}

if (is.null(module_name())) {
    C = structure(c(1L, 2L, 3L, 4L, 6L, 5L), .Dim = 2:3,
        .Dimnames = list(c("a", "b"), c("x", "y", "z")))

    Y = split(C, along=2, subsets=c('s1','s1','s2'))
    # $s1          $s2
    #   x y        a b   # each subset is split into a separate array
    # a 1 3        5 6
    # b 2 4

    Yref = structure(list(s1 = structure(1:4, .Dim = c(2L, 2L),
        .Dimnames = list(c("a", "b"), c("x", "y"))),
        s2 = structure(c(6L, 5L), .Dim = c(2L,1L),
        .Dimnames = list(c("a", "b"), "z"))), .Names = c("s1", "s2"))

    testthat::expect_equal(Y, Yref)
}
