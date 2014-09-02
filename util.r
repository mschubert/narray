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

#' Return \code{dimnames()} of an array respecting the number of dimensions
#'
#' @param X                An n-dimensional array
#' @param null.as.integer  Whether nameless dimensions should be \code{NULL} or numbered
#' @return                 A list of dimension names with length \code{length(ndim(X))}
dimnames = function(X, null.as.integer=FALSE) {
    if (is.list(X)) {
        if (is.null(names(X))) {
            if (null.as.integer)
                dn = c(1:length(X))
            else
                dn = list(NULL)
        }
        else
            dn = names(X)
    } else {
        X = as.array(X)
        if (is.null(base::dimnames(X)))
            dn = rep(list(NULL), length(dim(X)))
        else
            dn = base::dimnames(X)
        if (null.as.integer == TRUE)
            dn = lapply(1:length(dn), function(i) 
                if (is.null(dn[[i]])) 1:dim(X)[i] else dn[[i]])
    }

    dn
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

