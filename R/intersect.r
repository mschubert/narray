#' Intersects all passed arrays along a give dimension, and modifies them in place
#'
#' TODO: accept along=c(1,2,1,1...) [maybe list w/ vectors as well?]
#' TODO: accept data=env/list arg? [sig-comb/drug-tissue/assocs.r#62-65]
#'
#' @param ...    Arrays that should be intersected
#' @param along  The axis along which to intersect
#' @param envir  A list or environment to act upon
#' @param drop   Drop unused dimensions on result
#' @export
intersect = function(..., along=1, envir=parent.frame(), drop=FALSE) {
    dots = pryr::named_dots(...)

    # for `data.frame`s, replace the rownames by field that is referenced
    for (i in seq_along(dots)) {
        if (is.call(dots[[i]])) {
            if (along == 1 && is.data.frame(eval(dots[[i]][[2]], envir=envir))) {
                # as.data.frame: need rownames, not dplyr's
                df = as.data.frame(eval(dots[[i]][[2]], envir=envir)) 
                field = eval(dots[[i]], envir=envir)
                df$.rownames = rownames(df)
                rownames(df) = field
                names(dots)[i] = as.character(dots[[i]][[2]])
                dots[[i]] = df
            } else
                stop("calls can only reference `data.frame` fields with along=1")
        } else
            dots[[i]] = eval(dots[[i]], envir=envir)
    }

    dots = intersect_list(dots, along=along, drop=drop)

    # recover original rownames if we stored them separately
    for (name in names(dots))
        if (is.data.frame(dots[[name]]) && !is.null(dots[[name]]$.rownames)) {
            rownames(dots[[name]]) = dots[[name]]$.rownames
            dots[[name]]$.rownames = NULL
        }

    # modify the list or environment with the intersected results
    if (is.list(envir))
        assign(as.character(match.call()$envir),
               utils::modifyList(envir, dots), envir=parent.frame())
    else
        for (name in names(dots))
            assign(name, dots[[name]], envir=envir)
}

#' Intersects a lits of arrays for common dimension names
#'
#' @param l.     List of arrays to perform operations on
#' @param along  The axis along which to intersect
#' @param drop   Drop unused dimensions on result
#' @export
intersect_list = function(l., along=1, drop=FALSE) {
    if (!is.list(l.))
        stop("`intersect_list()` expects a list as first argument, found: ", class(l.))

    red_int = function(...) {
        Reduce(base::intersect, list(...))
    }

    common = do.call(red_int, dimnames(l., along=along))
    lapply(l., function(e) subset(e, index=common, along=along, drop=drop))
}
