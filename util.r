#' A multidimensional \code{which} function
#'
#' From: \url{http://www.r-bloggers.com/a-multidimensional-which-function/}
#'
#' @param A  N-dimensional logical array
#' @return   A matrix with indices where \code{A == TRUE}
which = function(A){
    if ( is.vector(A) ) return(which(A))
    d = dim(A)
    T = base::which(A) - 1
    nd = length(d)
    t( sapply(T, function(t){
        I = integer(nd)
        I[1] = t %% d[1]
        sapply(2:nd, function(j){
            I[j] <<- (t %/% prod(d[1:(j-1)])) %% d[j]
        })
        I
    }) + 1 )
}

#' base::dim, but returning 1 for vector
dim = function(x) {
    if (is.vector(x))
        length(x)
    else
        base::dim(x)
}

#' Assigns matrix element names by row- and column names
#'
#' @param X    An n-dimensional array
#' @param sep  Character to use when assembling names for each element
#' @return     A vector of name characters for each element of the array \code{X}
elementnames = function(X, sep=":") {
    apply(expand.grid(dimnames(X, null.as.integer=T)), 1, 
          function(x) paste(x,collapse=sep))
}

#' Reshapes \code{X} to be like \code{like}, including dimension names
#'
#' @param X     An n-dimensional array
#' @param like  An n-dimensional array whose form \code{X} should inherit
#' @return      An array with values of \code{X} and structure of \code{like}
like = function(X, like) {
    like[] = X
    like
}
