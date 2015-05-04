# Array programming utility functions
# Some tools to handle R^n matrices and perform operations on them
.b = import('../base')

#' A wrapper around reshape2::acast using a more intuitive formula syntax
#'
#' @param X              A data frame
#' @param formula        A formula: value [+ value2 ..] ~ axis1 [+ axis2 + axis n ..]
#' @param fill           Value to fill array with if undefined
#' @param fun.aggregate  Function to aggregate multiple values for the same position
#' @param ...            Additional arguments passed to reshape2::acast
#' @return               A structured array
construct = function(X, formula, fill=NULL, fun.aggregate=aggr_error, ...) {
    if (!is.data.frame(X) && is.list(X)) #TODO: check, names at level 1 = '.id'
    X = plyr::ldply(X, data.frame)
#TODO: convert nested list to data.frame first as well?
    dep_vars = all.vars(formula[[2]])
    indep_vars = all.vars(formula[[3]])

    form = as.formula(paste(indep_vars, collapse = "~"))
    res = sapply(dep_vars, function(v) reshape2::acast(
        as.data.frame(X), formula=form, value.var=v,
        fill=fill, fun.aggregate=fun.aggregate, ...
    ), simplify=FALSE)
    if (length(res) == 1) #TODO: drop_list in base?
        res[[1]]
    else
        res
}
