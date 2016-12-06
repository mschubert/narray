#' Reshapes \code{x} to be like \code{like}, including dimension names
#'
#' @param x     An n-dimensional array
#' @param like  An n-dimensional array whose form \code{X} should inherit
#' @return      An array with values of \code{X} and structure of \code{like}
#' @export
like = function(x, like) {
    like[] = x
    like
}
