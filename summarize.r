.b = import('../base')
.s = import('./split')
.m = import('./map')
.bi = import('./bind')
`%>%` = magrittr::`%>%`

#' Summarize a matrix analogous to a grouped df in dplyr
#'
#' @param x      A matrix
#' @param from   Names that match the dimension `along`
#' @param to     Names that this dimension should be summarized to
#' @param along  Along which axis to summarize
#' @param FUN    Which function to apply, default is throwing error on aggregation
#' @return       A summarized matrix as defined by `from`, `to`
summarize = function(x, along=1, to, from=dimnames(x)[[along]], FUN=aggr_error) {
    lookup = .b$match(dimnames(x)[[along]], from=from, to=to, na_rm=TRUE)
    x = subset(x, index=names(lookup), along=along)
    
    # aggregate the rest using fun
    .s$split(x, along=along, subsets=lookup) %>%
        lapply(function(j) .m$map(j, along, FUN)) %>%
        .bi$bind(along=along)
}

if (is.null(module_name())) {
    G = matrix(c(1,2,0,3,4,5), nrow=3,
               dimnames=list(c('A','B','C'), c('D','E')))

    W = summarize(G, along=1, from=rownames(G), to=c('a','b','b'), FUN=mean)
    #   D   E
    # a 1 3.0
    # b 1 4.5

    Wref = structure(c(1, 1, 3, 4.5), .Dim = c(2L, 2L),
                     .Dimnames = list(c("a", "b"), c("D", "E")))
    testthat::expect_equal(W, Wref)
}
