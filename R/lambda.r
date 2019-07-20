#' Lambda syntax for array iteration
#'
#' @param fml       A call prefixed with a tilde
#' @param along     A named vector which objects to subset (eg: c(x=1))
#' @param group     Not implemented
#' @param simplify  Return array instead of index+result if scalar
#' @param expand_grid  Use all combinations of indices (default: TRUE)
#' @param envir     Environment where variables can be found
#' @export
lambda = function(fml, along, group=c(), simplify=TRUE, expand_grid=TRUE, envir=parent.frame()) {
    if (fml[[1]] != "~")
        stop("lambda expression needs to start with a tilde character")
    call = fml[[2]]

    obj2dname = function(objname, along) {
        obj = get(objname, envir=envir)
        dimnames(obj, null_as_integer=TRUE, along=along)
    }
    dnames = mapply(obj2dname, objname=names(along), along=along, SIMPLIFY=FALSE)
    if (expand_grid)
        iter = do.call(expand.grid, c(dnames, list(stringsAsFactors=FALSE)))
    else
        iter = do.call(data.frame, c(dnames, list(stringsAsFactors=FALSE)))
    class(iter) = c("tbl_df", "tbl", class(iter))

    wrapper = function(row) {
        env = new.env(parent=envir)
        args = stats::setNames(as.list(iter[row,]), colnames(iter))
        for (i in seq_along(args)) {
            objname = names(args)[i]
            obj = get(objname, envir=envir)
            subs = subset(obj, index=args[[i]], along=along[objname], drop=TRUE)
            assign(objname, subs, envir=env)
        }
        tryCatch(eval(call, envir=env),
            error = function(e) {
                cur = paste(colnames(iter), as.character(iter[row,]), sep="=")
                newmsg = paste(conditionMessage(e), "@", paste(cur, collapse=", "))
                e$message = newmsg
                stop(e)
        })
    }
    pb = pb(nrow(iter))
    iter$result = lapply(seq_len(nrow(iter)), function(i) {
        re = wrapper(i)
        pb$tick()
        re
    })

    if (is.atomic(iter$result[[1]]) && length(iter$result[[1]]) == 1) {
        iter$result = simplify2array(iter$result)
        if (simplify) {
            axes = setdiff(colnames(iter), "result")
            join = paste(axes, collapse="+")

            if (length(axes) > 1)
                iter = construct(iter, stats::as.formula(paste("result ~", join)))
            else if (is.character(iter[,1]))
                iter = stats::setNames(iter$result, iter[,1])
            else
                iter = iter$result
        }
    }
    iter
}
