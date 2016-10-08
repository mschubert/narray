#' Binds arrays together disregarding names
#'
#' @param arrayList  A list of n-dimensional arrays
#' @param along      Along which axis to bind them together; default: new axis
#' @return           A joined array
#' @export
bind = function(arrayList, along=length(dim(arrayList[[1]]))+1) {
    re = do.call(function(...) abind::abind(..., along=along), arrayList)

    # is.null(...) required because R is stupid
    if (dim(re)[along] == length(arrayList) && !is.null(names(arrayList)))
        dimnames(re)[[along]] = names(arrayList)

    re
}
