#' Function to melt data.frame from one or multiple arrays
#'
#' @param ...       Array[s] or data.frame[s] to be melted
#' @param dimnames  List of names along the dimensions
#' @param na_rm     Remove rows with NAs
#' @return   data.frame with 'value' (or object names if multiple) indexed by axes
#' @export
melt = function(..., dimnames=NULL, na_rm=TRUE) {
    dots = lapply(named_dots(...), eval, envir=parent.frame())
    for (i in seq_along(dots)) {
        if (!is.null(dimnames))
            names(dimnames(dots[[i]])) = dimnames[1:length(dim(dots[[i]]))]
        dots[[i]] = melt_one(dots[[i]], na_rm=na_rm)
        if (length(dots) > 1)
            colnames(dots[[i]])[ncol(dots[[i]])] = names(dots)[i]
    }

    Reduce(function(a,b) merge(a,b,all=!na_rm), dots)
}

melt_one = function(x, dn=dimnames(x), na_rm=FALSE) {
    dns = expand.grid(dn, KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE)
    re = cbind(dns, value=c(x))
    if (na_rm)
        stats::na.omit(re)
    else
        re
}
