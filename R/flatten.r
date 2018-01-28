#' Flattens an array along an axis
#'
#' @param x         Array
#' @param along     Along which axis to bind them together (default: last)
#' @param name_sep  Which character to use for naming new arrays [default: NA, do not touch names]
#' @return          An array with n-1 dimensions
#' @export
flatten = function(x, along=-1, name_sep=NA) {
    if (along == -1)
        along = length(dim(x))

    re = split(x, along=along, drop=TRUE)

    if (!is.na(name_sep))
        re = mapply(function(x, n) {
            dimnames(x)[[along]] = paste(n, dimnames(x)[[along]], sep=name_sep)
            x
        }, re, names(re), SIMPLIFY=FALSE)

    stack(re, along=along)
}
