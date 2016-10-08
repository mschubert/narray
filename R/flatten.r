#' Flattens an array along an axis
#'
#' @param X         Array
#' @param along     Along which axis to bind them together
#' @param name_sep  Which character to use for naming new arrays [default: NA, do not touch names]
#' @return          An array with n-1 dimensions
#' @export
flatten = function(X, along, name_sep=NA) {
    re = split(X, along=along, drop=TRUE)

    if (!is.na(name_sep))
        re = mapply(function(x, n) {
            dimnames(x)[[along]] = paste(n, dimnames(x)[[along]], sep=name_sep)
            x
        }, re, names(re), SIMPLIFY=FALSE)

    stack(re, along=along)
}
