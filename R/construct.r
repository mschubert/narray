#' A wrapper around reshape2::acast using a more intuitive formula syntax
#'
#' The construct() function can be called either with the data.frame as the
#' first argument or the formula and then specify `data=<data.frame>`
#'
#' @param data           A data frame
#' @param formula        A formula: value ~ axis1 [+ axis2 + axis n ..]
#' @param fill           Value to fill array with if undefined
#' @param fun.aggregate  Function to aggregate multiple values for the same position; default: error
#' @param ...            Additional arguments passed to reshape2::acast
#' @return               A structured array
#' @export
construct = function(data, formula, fill=NULL, fun.aggregate=NULL, ...) {
    if (inherits(data, "formula")) {
        swap = data
        data = formula
        formula = swap
    }

    if (!is.data.frame(data))
        stop("`data` needs to reference a data.frame")

    dep_var = all.vars(formula[[2]])
    if (length(dep_var) != 1)
        stop("The dependent variable (left side of ~) needs to reference exactly one variable")

    indep_vars = all.vars(formula[[3]]) #TODO: if factor, include all levels in matrix
    form = stats::as.formula(paste(indep_vars, collapse = "~"))

    data = as.data.frame(data)
    axis_NA = apply(is.na(data[indep_vars]), 1, any)
    if (any(axis_NA)) {
        warning("Omitting ", sum(axis_NA), " rows where axis columns are NA")
        data = data[!axis_NA,]
    }

	`%catchm%` = function(a, b) {
		withCallingHandlers(
			a,
			message = function(w) b
		)
	}

    reshape2::acast(data,
                    formula = form,
		            value.var = dep_var,
                    fill = fill,
                    fun.aggregate = fun.aggregate,
                    ...) %catchm% stop("Do not know how to aggregate")
}
