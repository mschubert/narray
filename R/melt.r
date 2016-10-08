#' Function to melt data.frame from one or multiple arrays
#'
#' @param ...       Array[s] or data.frame[s] to be melted
#' @param dimnames  List of names along the dimensions (instead of `VarX`)
#' @param na_rm     Remove rows with NAs
#' @export
melt = function(..., dimnames=NULL, na_rm=TRUE) {
    l. = list(...)
    for (i in seq_along(l.)) {
        if (!is.null(dimnames))
            names(dimnames(l.[[i]])) = dimnames[1:length(dim(l.[[i]]))]
        l.[[i]] = reshape2::melt(l.[[i]], value.name=names(l.)[i], na.rm=na_rm)
    }

    Reduce(function(a,b) merge(a,b,all=!na_rm), l.)
}
