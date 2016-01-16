.s = import_('./stack')

#' Converts a list of character vectors to a logical matrix
#'
#' @param x  A list of character vectors
#' @return   A logical occurrence matrix
mask = function(x) {
    if (is.factor(x))
        x = as.character(x)

    vectorList = lapply(x, function(xi) setNames(rep(TRUE, length(xi)), xi))
    .s$stack(vectorList, fill=FALSE)
}

if (is.null(module_name())) {
    library(testthat)

    F = list(a=c('e1','e2'),b='e1',c='e2')

    Z = mask(F)
    #      e1    e2
    # a  TRUE  TRUE
    # b  TRUE FALSE
    # c FALSE  TRUE

    Zref = structure(c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE), .Dim = c(3L,
    2L), .Dimnames = list(c("a", "b", "c"), c("e1", "e2")))
    expect_equal(Z, Zref)
}
