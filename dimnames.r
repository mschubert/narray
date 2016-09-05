#' Return dimension names of an array respecting the number of dimensions
#'
#' Act on each element if 'x' is a list
#'
#' @param x      An n-dimensional array
#' @param along  Limit to dimension (default: all)    
#' @param null.as.integer  Whether nameless dimensions should be \code{NULL} or numbered
#' @param drop   Drop list of only one axis requested (default: TRUE)
#' @return       A list of dimension names with length \code{length(ndim(X))}
dimnames = function(x, along=TRUE, null.as.integer=FALSE, drop=TRUE) {
    UseMethod("dimnames")
}

dimnames.data.frame = function(x, along=TRUE, null.as.integer=FALSE, drop=TRUE) {
    dimnames(as.matrix(x), along=along, null.as.integer=null.as.integer, drop=drop)
}

dimnames.list = function(x, along=TRUE, null.as.integer=FALSE, drop=TRUE) {
    lapply(x, function(x) dimnames(x, along=along, null.as.integer=null.as.integer, drop=drop))
}

dimnames.default = function(x, along=TRUE, null.as.integer=FALSE, drop=TRUE) {
#    if (!is.data.frame(x))
        x = as.array(x)

    dn = base::dimnames(x)
    if (is.null(dn))
        dn = rep(list(NULL), length(dim(x)))

    if (null.as.integer == TRUE)
        dn = lapply(1:length(dn), function(i) {
            if (is.null(dn[[i]]))
                1:dim(x)[i]
            else
                dn[[i]]
        })

    if (!identical(along, TRUE) && length(along) == 1 && drop==TRUE)
        dn[[along]]
    else
        dn[along]
}

if (is.null(module_name())) {
    library(testthat)

    dn = list(c('a','b'),c('x','y'))
    a = setNames(1:2, dn[[1]])
    A = matrix(1:4, nrow=2, ncol=2, dimnames=dn)
    DF = structure(list(y=3:4, z=c(6,5), x=1:2, A=c("b", "a")),
            .Names=c("y","z","x","A"), row.names=1:2, class="data.frame")
    ll = list(a=a, A=A, DF=DF)

    # vector
    expect_equal(dimnames(a), dn[1])
    expect_equal(dimnames(a, along=1), dn[[1]])
    expect_equal(dimnames(a, along=1, drop=FALSE), dn[1])
    expect_error(dimnames(a, along=2))

    # matrix
    expect_equal(dimnames(A), dn)
    expect_equal(dimnames(A, along=2), dn[[2]])

    # data.frame
    expect_equal(dimnames(DF, along=1), as.character(1:2))

    # list
    dnl = dimnames(ll)
    expect_equal(dnl$a, dn[1])
    expect_equal(dnl$A, dn)
    dnl1 = dimnames(ll, along=1)
    expect_equal(dnl1$a, dnl1$A, dn[[1]])
}
