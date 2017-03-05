#' Binds arrays together disregarding names
#'
#' @param arrayList  A list of n-dimensional arrays
#' @param along      Along which axis to bind them together; default: new axis
#' @return           A joined array
#' @export
bind = function(arrayList, along=length(dim(arrayList[[1]]))+1) {
    #TODO: this is borrowed from stack, refactor
    # for vectors: if along=1 row vecs, along=2 col vecs, etc.
    if (all(is.null(unlist(lapply(arrayList, base::dim))))) {
        if (along == 1)
            arrayList = lapply(seq_along(arrayList), function(i) {
                re = t(as.matrix(arrayList[[i]]))
                rownames(re) = names(arrayList)[i]
                re
            })
        else if (along == 2)
            arrayList = lapply(seq_along(arrayList), function(i) {
                re = as.matrix(arrayList[[i]])
                colnames(re) = names(arrayList)[i]
                re
            })
    }
    # end TODO:

    re = do.call(function(...) abind::abind(..., along=along), arrayList)

    # is.null(...) required because R is stupid
    if (dim(re)[along] == length(arrayList) && !is.null(names(arrayList)))
        dimnames(re)[[along]] = names(arrayList)

    re
}
