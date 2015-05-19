.b = import('../base')
.u = import('./util')

#' Intersects all passed arrays along a give dimension, and modifies them in place
#'
#' @param ...    Arrays that should be intersected
#' @param along  The axis along which to intersect
#' @param data   A list or environment to act upon
#TODO: accept along=c(1,2,1,1...) [maybe list w/ vectors as well?]
intersect = function(..., along=1, data=parent.frame(), drop=FALSE) {
    l. = list(...)
    varnames = match.call(expand.dots=FALSE)$...
    namesalong = lapply(l., function(f) .u$dimnames(f)[[along]])
    common = do.call(.b$intersect, namesalong)
    for (i in seq_along(l.)) {
        dims = as.list(rep(T, length(dim(l.[[i]]))))
        dims[[along]] = common
        assign(as.character(varnames[[i]]),
               value = abind::asub(l.[[i]], dims, drop=drop),
               envir = data)
    }
}

if (is.null(module_name())) {
    A = matrix(1:4, nrow=2, ncol=2, dimnames=list(c('a','b'),c('x','y')))
    C = structure(c(1L, 2L, 3L, 4L, 6L, 5L), .Dim = 2:3,
        .Dimnames = list(c("a", "b"), c("x", "y", "z")))
    E = C[,c(2,3,1)]
    DF = as.data.frame(E[1,,drop=FALSE])
    DFref = DF

    intersect(A, E, along=2)
    # > A         > E
    #   x y         x y   # along dimension 2, all arrays have same extent
    # a 1 3       a 1 3   # and same order of names; this function modifies
    # b 2 4       b 2 4   # values in-place
    
    AEref = structure(1:4, .Dim = c(2L, 2L),
                      .Dimnames = list(c("a", "b"), c("x", "y")))
    testthat::expect_equal(A, AEref)
    testthat::expect_equal(E, AEref)

    intersect(A, DF, along=1)
    # > A        > DF
    #   x y        y z x
    # a 1 3      a 3 6 1

    testthat::expect_is(A, "matrix")
    testthat::expect_is(DF, "data.frame")
    testthat::expect_equal(A, AEref[1,,drop=FALSE])
    testthat::expect_equal(DF, DFref)

    intersect(A, DF, along=2, drop=TRUE)
    testthat::expect_is(A, "integer")
    testthat::expect_is(DF, "list")
    testthat::expect_equal(A, c(x=1, y=3))
    testthat::expect_equal(DF, list(x=1, y=3))
}
