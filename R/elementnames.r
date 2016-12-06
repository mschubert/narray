#' Assigns matrix element names by row- and column names
#'
#' @param X    An n-dimensional array
#' @param sep  Character to use when assembling names for each element
#' @return     A vector of name characters for each element of the array \code{X}
#' @export
elementnames = function(X, sep=":") {
    apply(expand.grid(dimnames(X, null_as_integer=TRUE)), 1, 
          function(x) paste(x,collapse=sep))
}
