.b = import('../base')

#' Intersects all passed arrays along a give dimension, and modifies them in place
#'
#' @param ...    Arrays that should be intersected
#' @param along  The axis along which to intersect
#' @param data   A list or environment to act upon
#TODO: accept along=c(1,2,1,1...) [maybe list w/ vectors as well?]
intersect = function(..., along=1, data=parent.frame()) {
    l. = list(...)
    varnames = match.call(expand.dots=FALSE)$...
    namesalong = lapply(l., function(f) dimnames(as.array(f))[[along]])
    common = do.call(.b$intersect, namesalong)
    for (i in seq_along(l.)) {
        dims = as.list(rep(T, length(dim(l.[[i]]))))
        dims[[along]] = common
        assign(as.character(varnames[[i]]),
               value = abind::asub(l.[[i]], dims),
               envir = data)
    }
}
