#' Intersects a lits of arrays for common dimension names
#'
#' @param l.     List of arrays to perform operations on
#' @param along  The axis along which to intersect
#' @param drop   Drop unused dimensions on result
#' @param fail_if_empty  Stop if intersection yields empty set
#' @export
intersect_list = function(l., along=1, drop=FALSE, fail_if_empty=TRUE) {
    if (!is.list(l.))
        stop("`intersect_list()` expects a list as first argument, found: ",
             class(l.))

    red_int = function(...) Reduce(base::intersect, list(...))

    common = do.call(red_int, dimnames(l., along=along))
    if (length(common) == 0 && fail_if_empty)
        stop("Intersection is empty and fail_if_empty is TRUE")

    lapply(l., function(e) subset(e, index=common, along=along, drop=drop))
}
