.b = import('../base')
.s = import('./split')

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
        lapply(function(j) map(j, along, FUN)) %>%
        do.call(function(j) abind::abind(j, along=along), .)
}
