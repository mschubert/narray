#' Converts a logical matrix to a list of character vectors
#'
#' This currently only supports x with only one non-zero element
#'
#' @param x      A logical matrix
#' @param along  Which axis to spread mask on
#' @return       A character vector or list thereof
#' @export
collect = function(x, along=2) {
    map(x, along=along, function(i) dimnames(x, along=along)[i])
}
