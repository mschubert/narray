# Array programming utility functions
# Some tools to handle R^n matrices and perform operations on them
import('../base/operators')

#' A wrapper around reshape2::acast using a more intuitive formula syntax
#'
#' @param formula        A formula: value [+ value2 ..] ~ axis1 [+ axis2 + axis n ..]
#' @param data           A data frame (TODO: handle envs, NULL, etc.)
#' @param fill           Value to fill array with if undefined
#' @param fun.aggregate  Function to aggregate multiple values for the same position; default: error
#' @param ...            Additional arguments passed to reshape2::acast
#' @return               A structured array
construct = function(formula, data, fill=NULL, fun.aggregate=NULL, ...) {
    if (!is.data.frame(data) && is.list(data)) #TODO: check, names at level 1 = '.id'
        data = plyr::ldply(data, data.frame)

    dep_vars = all.vars(formula[[2]])
    indep_vars = all.vars(formula[[3]])
    form = as.formula(paste(indep_vars, collapse = "~"))

    res = sapply(dep_vars, function(v) reshape2::acast(
        as.data.frame(data), formula=form, value.var=v,
        fill=fill, fun.aggregate=fun.aggregate, ...
    ), simplify=FALSE) %catchm% stop("Do not know how to aggregate")

    if (length(res) == 1) #TODO: drop_list in base?
        res[[1]]
    else
        res
}

if (is.null(module_name())) {
    DF = data.frame(expand.grid(LETTERS[1:3], LETTERS[4:5])[-3,], value=1:5)

    G = construct(value ~ Var1 + Var2, data=DF, fun.aggregate=sum)
    #   D E
    # A 1 3
    # B 2 4
    # C 0 5

    Gref = structure(c(1L, 2L, 0L, 3L, 4L, 5L), .Dim = c(3L, 2L),
                     .Dimnames = list(c("A", "B", "C"), c("D", "E")))
    testthat::expect_equal(G, Gref)

    DFa = rbind(DF, c("A","D",6)) # ambiguous row
    testthat::expect_error(construct(value ~ Var1 + Var2, data=DFa))
}
