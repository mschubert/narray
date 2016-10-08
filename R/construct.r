#' A wrapper around reshape2::acast using a more intuitive formula syntax
#'
#' @param formula        A formula: value [+ value2 ..] ~ axis1 [+ axis2 + axis n ..]
#' @param data           A data frame (TODO: handle envs, NULL, etc.)
#' @param fill           Value to fill array with if undefined
#' @param fun.aggregate  Function to aggregate multiple values for the same position; default: error
#' @param ...            Additional arguments passed to reshape2::acast
#' @return               A structured array
#' @export
construct = function(formula, data, fill=NULL, fun.aggregate=NULL, ...) {
    if (!is.data.frame(data) && is.list(data)) #TODO: check, names at level 1 = '.id'
        data = plyr::ldply(data, data.frame)

    dep_vars = all.vars(formula[[2]])
    indep_vars = all.vars(formula[[3]]) #TODO: if factor, include all levels in matrix
    form = as.formula(paste(indep_vars, collapse = "~"))

    data = as.data.frame(data)
    axis_NA = apply(is.na(data[indep_vars]), 1, any)
    if (any(axis_NA)) {
        warning("Omitting ", sum(axis_NA), " rows where axis columns are NA")
        data = data[!axis_NA,]
    }

    withCallingHandlers(
        res = sapply(dep_vars, function(v) reshape2::acast(data, formula=form,
            value.var=v, fill=fill, fun.aggregate=fun.aggregate, ...
        ), simplify=FALSE),
        message = stop("Do not know how to aggregate")
    )

    if (length(res) == 1) #TODO: drop_list in base?
        res[[1]]
    else
        res
}
