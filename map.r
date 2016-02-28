.s = import_('./subset')
.b = import_('./bind')

#' Apply function that preserves order of dimensions
#'
#' @param X        An n-dimensional array
#' @param along    Along which axis to apply the function
#' @param FUN      A function that maps a vector to the same length or a scalar
.map_simple = function(X, along, FUN, drop=TRUE) {
    if (is.vector(X) || length(dim(X))==1)
        return(FUN(X))

    preserveAxes = c(1:length(dim(X)))[-along]
    Y = as.array(apply(X, preserveAxes, FUN))
    if (length(dim(Y)) < length(dim(X)))
        Y = array(Y, dim=c(1, dim(Y)), dimnames=c(list(NULL), dimnames(Y)))

    Y = aperm(Y, match(seq_along(dim(Y)), c(along, preserveAxes)))
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
        .map_simple(.s$subset(X, f), along, FUN, drop=FALSE))

    # assemble results together
    Y = .b$bind(resultList, along=along)
    if (dim(Y)[along] == nsubsets)
        base::dimnames(Y)[[along]] = lsubsets
    else if (dim(Y)[along] == dim(X)[along])
        base::dimnames(Y)[[along]] = base::dimnames(X)[[along]]

    if (drop)
        drop(Y)
    else
        Y
}

if (is.null(module_name())) {
    library(testthat)

    D = structure(c(1L, 2L, 3L, 4L, NA, NA, 1L, 2L, 3L, 4L, 6L, 5L),
                  .Dim = c(2L, 3L, 2L), .Dimnames = list(c("a", "b"),
                  c("x", "y", "z"), c("m", "n")))

    X = map(D, along=1, function(x) sum(x, na.rm=TRUE))
    #    m  n
    #  x 3  3
    #  y 7  7
    #  z 0 11
    Xref = structure(c(3L, 7L, 0L, 3L, 7L, 11L), .Dim = c(3L, 2L),
                     .Dimnames = list(c("x", "y", "z"), c("m", "n")))
    expect_equal(X, Xref)

    X3 = map(D, along=3, sum)
    #    x y  z
    #  a 2 6 NA
    #  b 4 8 NA
    X3ref = structure(c(2L, 4L, 6L, 8L, NA, NA), .Dim = 2:3, .Dimnames = list(
                      c("a", "b"), c("x", "y", "z")))
    expect_equal(X3, X3ref)

    l = letters
    id = function(x) x
    a = array(1:(2*3*4*5),
              dim = c(2,3,4,5),
              dimnames = list(l[1:2], l[3:5], l[6:9], l[10:14]))

    expect_equal(dim(.map_simple(a, along=1, id)),
                 dim(.map_simple(a, along=2, id)),
                 dim(.map_simple(a, along=3, id)),
                 dim(.map_simple(a, along=4, id)),
                 dim(map(a, along=1, id)),
                 dim(map(a, along=2, id)),
                 dim(map(a, along=3, id)),
                 dim(map(a, along=5, id)),
                 dim(a))

    fx = function(x) sum(x)

    expect_equal(dim(.map_simple(a, along=1, fx, drop=FALSE)),
                 dim(map(a, along=1, fx, drop=FALSE)),
                 c(1,3,4,5))

    expect_equal(dim(.map_simple(a, along=2, fx, drop=FALSE)),
                 dim(map(a, along=2, fx, drop=FALSE)),
                 c(2,1,4,5))

    expect_equal(dim(.map_simple(a, along=3, fx, drop=FALSE)),
                 dim(map(a, along=3, fx, drop=FALSE)),
                 c(2,3,1,5))

    expect_equal(dim(.map_simple(a, along=4, fx, drop=FALSE)),
                 dim(map(a, along=4, fx, drop=FALSE)),
                 c(2,3,4,1))

    m = a[,,1,1] # 2 3
    expect_equal(dim(.map_simple(m, along=1, fx, drop=FALSE)),
                 dim(map(m, 1, fx, drop=FALSE)),
                 c(1,3))

    expect_equal(dim(.map_simple(m, along=2, fx, drop=FALSE)),
                 dim(map(m, 1, fx, drop=FALSE)),
                 c(2,1))
}
