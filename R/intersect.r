#' Intersects all passed arrays along a give dimension, and modifies them in place
#'
#' TODO: accept along=c(1,2,1,1...) [maybe list w/ vectors as well?]
#' TODO: accept data=env/list arg? [sig-comb/drug-tissue/assocs.r#62-65]
#'
#' @param ...    Arrays that should be intersected
#' @param along  The axis along which to intersect
#' @param envir  A list or environment to act upon
#' @param drop   Drop unused dimensions on result
#' @param fail_if_empty  Stop if intersection yields empty set
#' @export
intersect = function(..., along=1, envir=parent.frame(), drop=FALSE,
                     fail_if_empty=TRUE) {

    dots = named_dots(...)
    df_store = list()

    # for `data.frame`s, replace the rownames by field that is referenced
    for (i in seq_along(dots)) {
        if (is.call(dots[[i]])) {
            mydf = eval(dots[[i]][[2]], envir=envir)
            if (is.data.frame(mydf)) {
                # replace dots element with index that we can intersect
                df_name = as.character(dots[[i]][[2]])
                df_store[[df_name]] = mydf
                dots[[i]] = stats::setNames(1:nrow(mydf),
                                            eval(dots[[i]], envir=envir))
                names(dots)[i] = df_name
            }
            else
                stop("calls can only reference `data.frame` fields")
        } else
            dots[[i]] = eval(dots[[i]], envir=envir)
    }

    dots = intersect_list(dots, along=along, drop=drop,
                          fail_if_empty=fail_if_empty)

    # recover original rownames if we stored them separately
    for (name in names(df_store))
        dots[[name]] = df_store[[name]][dots[[name]],]

    # modify the list or environment with the intersected results
    if (is.list(envir))
        assign(as.character(match.call()$envir),
               utils::modifyList(envir, dots), envir=parent.frame())
    else
        for (name in names(dots))
            assign(name, dots[[name]], envir=envir)
}
