subsets = function(X, along, subsets) {
    if (length(subsets) != dim(as.array(X))[along])
        stop("subset length must match X dimension on along axis")

    if (any(is.na(subsets)))
        stop("found NA in subsets, exiting")
}

along = function(X, along) {
    if (any(duplicated(dimnames(X)[[along]])))
        stop("duplicated names found along mapping axis, exiting")

    if (is.character(along))
        assign('along', which(names(dim(X)) == along, envir=parent.frame()))
}

x = function(X, to.array, classes=NULL, envir=parent.frame()) {
    if (!is.null(classes) && !class(X) %in% classes)
        stop(paste("class of X not in classes:", classes))

    if (to.array)
        assign('X', as.array(X), envir=envir)
}

all = function(X, along, subsets, x.to.array=FALSE, envir=parent.frame()) {
    along(X, along)
    subsets(X, along, subsets)
    x(X, x.to.array, envir=envir)
}
