# array programming utility functions
# some tools to handle R^n matrices and perform operations on them
library(abind)
library(reshape2)
library(plyr)
library(modules)
base = import('base')

#TODO: make sure there is no NA in the combined names
#TODO:? would be faster if just call abind() when there is nothing to sort
array_stack = function(arrayList, along=length(dim(arrayList[[1]]))+1, fill=NA, like=NA) {
    if (!is.list(arrayList))
        stop(paste("arrayList needs to be a list, not a", class(arrayList)))
    if (length(arrayList) == 1)
        return(arrayList[[1]])

    # union set of dimnames along a list of arrays (TODO: better way?)
    arrayList = lapply(arrayList, function(x) as.array(drop(x)))

    newAxis = FALSE
    if (along > length(dim(arrayList[[1]])))
        newAxis = TRUE

    if (identical(like, NA)) {
        dn = lapply(arrayList, array_dimnames)
        dimNames = lapply(1:length(dn[[1]]), function(j) 
            unique(c(unlist(sapply(1:length(dn), function(i) 
                dn[[i]][[j]]
            ))))
        )
        ndim = sapply(1:length(dimNames), function(i)
            if (!is.null(dimNames[[i]]))
                length(dimNames[[i]]) 
            else
                max(sapply(arrayList, function(j) dim(j)[i]))
        )

        # if creating new axis, amend ndim and dimNames
        if (newAxis) {
            dimNames = c(dimNames, list(names(arrayList)))
            ndim = c(ndim, length(arrayList))
        }

        result = array(fill, dim=ndim, dimnames=dimNames)
    } else {
        result = array(fill, dim=dim(like), dimnames=dimnames(like))
    }

    # create stack with fill=fill, replace each slice with matched values of arrayList
    for (i in array_dimnames(arrayList, null.as.integer=T)) {
        dm = array_dimnames(arrayList[[i]], null.as.integer=T)
        if (newAxis)
            dm[[along]] = i
        result = do.call("[<-", c(list(result), dm, list(arrayList[[i]])))
    }
    result
}

array_bind = function(arrayList, along=length(dim(arrayList[[1]]))+1) {
#TODO:
# check names?
# call bind when no stacking needed automatically?
    do.call(function(f) abind(f, along=along), arrayList)
}

# function to KEEP!
array_filter = function(X, along, FUN, subsets=rep(1,dim(X)[along]), na.rm=F) {
    X = as.array(X)
    # apply the function to get a subset mask
    mask = array_map(X, along, function(x) FUN(x), subsets)
    if (mode(mask) != 'logical' || dim(mask)[1] != length(unique(subsets)))
        stop("FUN needs to return a single logical value")

#    for (mcol in seq_along(ncol(mask)))
        for (msub in rownames(mask))
            if (!mask[msub])
                X[subsets==msub] = NA #FIXME: work for matrices as well

    if (na.rm)
        base$na.col.omit(na.omit(X))
    else
        X
}

array_unmelt = function(X, value, axes) {
    acast(as.data.frame(X), formula=as.formula(paste(axes, collapse = "~")), value.var=value)
}

array_subset = function(X, ll) {
    asub(X, ll)
}

# http://www.r-bloggers.com/a-multidimensional-which-function/
array_which = function(A){
    if ( is.vector(A) ) return(which(A))
    d = dim(A)
    T = which(A) - 1
    nd = length(d)
    t( sapply(T, function(t){
        I = integer(nd)
        I[1] = t %% d[1]
        sapply(2:nd, function(j){
            I[j] <<- (t %/% prod(d[1:(j-1)])) %% d[j]
        })
        I
    }) + 1 )
}

# apply function that preserves order of dimensions
array_map_simple = function(X, along, FUN) { #TODO: replace this by alply?
    if (is.vector(X) || length(dim(X))==1)
        return(FUN(X))

    preserveAxes = c(1:length(dim(X)))[-along]
    Y = apply(X, preserveAxes, FUN)
    if (is.vector(Y)) {
        if (along == 1) {
            newdim = c(1, length(Y))
            newdimnames = list(NULL, names(Y))
        } else {
            newdim = c(length(Y), 1)
            newdimnames = list(names(Y), NULL)
        }
        array(Y, dim=newdim, dimnames=newdimnames)
    } else {
        aperm(Y, c(along, preserveAxes))
    }
}

array_map = function(X, along, FUN, subsets=rep(1,dim(X)[along])) {
    stopifnot(length(subsets) == dim(X)[along])

    X = as.array(X)
    subsets = as.factor(subsets)
    lsubsets = levels(subsets)
    nsubsets = length(lsubsets)

    # create a list to index X with each subset
    subsetIndices = rep(list(rep(list(TRUE), length(dim(X)))), nsubsets)
    for (i in 1:nsubsets)
        subsetIndices[[i]][[along]] = (subsets==lsubsets[i])

    # for each subset, call mymap
    resultList = lapply(subsetIndices, function(f)
        array_map_simple(array_subset(X, f), along, FUN))
#    resultList = lapply(subsetIndices, function(x) alply(array_subset(X, f), along, FUN)) FIXME:

    # assemble results together
    Y = do.call(function(...) abind(..., along=along), resultList)
    if (dim(Y)[along] == dim(X)[along])
        dimnames(Y)[[along]] = dimnames(X)[[along]]
    else if (dim(Y)[along] == nsubsets)
         dimnames(Y)[[along]] = lsubsets
    drop(Y)

}

array_split = function(X, along, subsets=c(1:dim(X)[along])) {
    X = as.array(X)
#TODO: check if names unique, otherwise weird error
    if (is.character(along))
        along = which(names(dim(X)) == along)

    stopifnot(length(subsets)==dim(X)[along])

    usubsets = unique(subsets)
    lus = length(usubsets)
    idxList = rep(list(rep(list(TRUE), length(dim(X)))), lus)

    for (i in 1:lus)
        idxList[[i]][[along]] = subsets==usubsets[i]

    if (length(usubsets)==dim(X)[along])
        lnames = dimnames(X)[[along]]
    else
        lnames = usubsets
    setNames(lapply(idxList, function(ll) array_subset(X, ll)), lnames)
}

array_dimnames = function(X, null.as.integer=FALSE) {
    if (is.list(X)) {
        if (is.null(names(X))) {
            if (null.as.integer)
                dn = c(1:length(X))
            else
                dn = list(NULL)
        }
        else
            dn = names(X)
    } else {
        X = as.array(X)
        if (is.null(dimnames(X)))
            dn = rep(list(NULL), length(dim(X)))
        else
            dn = dimnames(X)
        if (null.as.integer == TRUE)
            dn = lapply(1:length(dn), function(i) 
                if (is.null(dn[[i]])) 1:dim(X)[i] else dn[[i]])
    }

    dn
}

# assigns matrix element names by row- and column names
array_elementnames = function(X, sep=":") {
    apply(expand.grid(array_dimnames(X, null.as.integer=T)), 1, 
          function(x) paste(x,collapse=sep))
}

#TODO: common.axis=T/F, specify the axis of each element
array_intersect = function(..., along=1) {
    l. = list(...)
    varnames = match.call(expand.dots=FALSE)$...
    namesalong = lapply(l., function(f) dimnames(as.array(f))[[along]])
    common = do.call(base$intersect, namesalong)
    for (i in seq_along(l.)) {
        dims = as.list(rep(T, length(dim(l.[[i]]))))
        dims[[along]] = common
        assign(as.character(varnames[[i]]), value=asub(l.[[i]], dims), envir=parent.frame())
    }
}

like = function(X, like) {
    like[] = X
    like
}

#TODO: common.axis=T/F, specify the axis of each element
intersect_list = function(x, along=1) {
    re = list()
    namesalong = lapply(x, function(f) dimnames(as.array(f))[[along]])
    common = do.call(base$intersect, namesalong)
    for (i in seq_along(x)) {
        dims = as.list(rep(T, length(dim(x[[i]]))))
        dims[[along]] = common
        re[[names(x)[i]]] = asub(x[[i]], dims)
    }
    re
}

