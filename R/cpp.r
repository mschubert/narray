#' test
#'
#' @export
cpp = function() {
    A = matrix(1:4, nrow=2, ncol=2, dimnames=list(c('a','b'),c('x','y')))
    B = matrix(5:6, nrow=2, ncol=1, dimnames=list(c('b','a'),'z'))
    arlist = list(A, B)

    cpp_stack(arlist)
}
