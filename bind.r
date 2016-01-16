#' Binds arrays together disregarding names
#'
#' @param arrayList  A list of n-dimensional arrays
#' @param along      Along which axis to bind them together; default: new axis
#' @return           A joined array
bind = function(arrayList, along=length(dim(arrayList[[1]]))+1) {
    if (along == 1) # abind does not handle these cases well
        do.call(rbind, arrayList)
    else if (along == 2)
        do.call(cbind, arrayList)
    else
        do.call(function(...) abind::abind(..., along=along), arrayList)
}
