#' Subsets an array using a list with indices or names
#'
#' @param X      The array to subset
#' @param index  A list of vectors to use for subsetting, or vector if along is given
#' @param along  Along which dimension to subset if index is a vector; default is last dimension
#' @return       The subset of the array
subset = function(X, index, along=NULL, drop=FALSE) {
    if (!is.list(index)) {
        ldaX = length(dim(as.array(X)))
        tmp = rep(list(TRUE), ldaX)
        if (is.null(along))
            along = ldaX
        tmp[[along]] = index
        index = tmp
    }

    abind::asub(X, index, drop=drop)
}

if (is.null(module_name())) {
    A = matrix(1:10, ncol=2)
    b = 1:5

    testthat::expect_equal(subset(A, c(1,2), along=1), A[1:2,])
    testthat::expect_equal(subset(A, c(1,2), along=2), A)

    testthat::expect_equal(subset(b, 1:5 %in% c(2,4,5), along=1), subset(A[,1], c(2,4,5)))
    testthat::expect_error(subset(b, 1, along=2))
}
