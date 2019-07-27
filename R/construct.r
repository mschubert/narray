#' Transform a data.frame with axes and value into an array
#'
#' The construct() function can be called either with the data.frame as the
#' first argument or the formula and then specify `data=<data.frame>`
#'
#' @param data           A data frame
#' @param formula        A formula: value ~ axis1 [+ axis2 + axis n ..]
#' @param fill           Value to fill array with if undefined
#' @param name_axes      Keep column names of `data` as axis names
#' @return               A structured array
#' @export
construct = function(data, formula=guess_structure(data), fill=NA,
                     name_axes=TRUE) {
    if (!is.data.frame(data))
        stop("`data` needs to reference a data.frame")

    dep_var = all.vars(formula[[2]])
    if (length(dep_var) != 1)
        stop("The dependent variable (left side of ~) needs to reference exactly one variable")
    indep_vars = all.vars(formula[[3]]) # include empty factor levels here?
    axis_NA = apply(is.na(data[indep_vars]), 1, any) # taking quite a long time
    if (any(axis_NA)) {
        warning("Omitting ", sum(axis_NA), " rows where axis columns are NA")
        data = data[!axis_NA,]
    }
    axes = data[indep_vars]
    values = data[[dep_var]]
    if (any(duplicated(axes))) # taking quite a long time
        stop("Duplicated entries in `data` are not allowed")

    dimNames = lapply(axes, unique)
    ndim = sapply(dimNames, length)

    order_df = t(mapply(base::match, axes, dimNames) - 1)
    mult = c(1, rev(cumprod(ndim[-length(ndim)])))
    ar_idx = colSums(order_df * mult) + 1

    idx = base::rep(fill, prod(ndim))
    idx[ar_idx] = values
    array(idx, dim=unname(ndim), dimnames=dimNames)
}
