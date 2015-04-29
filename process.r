# Array programming utility functions
# Some tools to handle R^n matrices and perform operations on them
library(methods) # abind bug: relies on methods::Quote, which is not loaded from Rscript
library(dplyr)
.b = import('base')
import('./util', attach=T)
.check = import('./checks')

#' Stacks arrays while respecting names in each dimension
#'
#' @param arrayList  A list of n-dimensional arrays
#' @param along      Which axis arrays should be stacked on (default: new axis)
#' @param fill       Value for unknown values (default: \code{NA})
#' @param like       Array whose form/names the return value should take
#' @return           A stacked array, either n or n+1 dimensional
stack = function(arrayList, along=length(dim(arrayList[[1]]))+1, fill=NA, like=NA) {
#TODO: make sure there is no NA in the combined names
#TODO:? would be faster if just call abind() when there is nothing to sort
    if (!is.list(arrayList))
        stop(paste("arrayList needs to be a list, not a", class(arrayList)))
    arrayList = arrayList[!is.null(arrayList)]
    if (length(arrayList) == 0)
        stop("No element remaining after removing NULL entries")
    if (length(arrayList) == 1)
        return(arrayList[[1]])

    # union set of dimnames along a list of arrays (TODO: better way?)
    arrayList = lapply(arrayList, function(x) as.array(x))

    newAxis = FALSE
    if (along > length(dim(arrayList[[1]])))
        newAxis = TRUE

    if (identical(like, NA)) {
        dn = lapply(arrayList, dimnames)
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
        result = array(fill, dim=dim(like), dimnames=base::dimnames(like))
    }

    # create stack with fill=fill, replace each slice with matched values of arrayList
    for (i in dimnames(arrayList, null.as.integer=T)) {
        dm = dimnames(arrayList[[i]], null.as.integer=T)
        if (any(is.na(unlist(dm))))
            stop("NA found in array names, do not know how to stack those")
        if (newAxis)
            dm[[along]] = i
        result = do.call("[<-", c(list(result), dm, list(arrayList[[i]])))
    }
    result
}

#' Binds arrays together disregarding names
#'
#' @param arrayList  A list of n-dimensional arrays
#' @param along      Along which axis to bind them together
#' @return           A joined array
bind = function(arrayList, along=length(dim(arrayList[[1]]))+1) {
#TODO: check names?, call bind when no stacking needed automatically?
#TODO: data.table::rbindlist?
    do.call(function(f) abind::abind(f, along=along), arrayList)
}

#' Function to discard subsets of an array (NA or drop)
#'
#' @param X        An n-dimensional array
#' @param along    Along which axis to apply \code{FUN}
#' @param FUN      Function to apply, needs to return \code{TRUE} (keep) or \code{FALSE}
#' @param subsets  Subsets that should be used when applying \code{FUN}
#' @param na.rm    Whether to omit columns and rows with \code{NA}s
#' @return         An array where filtered values are \code{NA} or dropped
filter = function(X, along, FUN, subsets=rep(1,dim(X)[along]), na.rm=FALSE) {
    .check$all(X, along, subsets)

    X = as.array(X)
    # apply the function to get a subset mask
    mask = as.array(map(X, along, function(x) FUN(x), subsets)) #FIXME: map should have drop=T/F
    if (mode(mask) != 'logical' || dim(mask)[1] != length(unique(subsets)))
        stop("FUN needs to return a single logical value")

#    for (mcol in seq_along(ncol(mask)))
        for (msub in rownames(mask))
            if (!mask[msub])
                X[subsets==msub] = NA #FIXME: work for matrices as well

    if (na.rm)
        .b$omit$na.col(na.omit(X))
    else
        X
}

#' A wrapper around reshape2::acast using a more intuitive formula syntax
#'
#' @param X              A data frame
#' @param formula        A formula: value [+ value2 ..] ~ axis1 [+ axis2 + axis n ..]
#' @param fill           Value to fill array with if undefined
#' @param fun.aggregate  Function to aggregate multiple values for the same position
#' @param ...            Additional arguments passed to reshape2::acast
#' @return               A structured array
construct = function(X, formula, fill=NULL, fun.aggregate=aggr_error, ...) {
    if (!is.data.frame(X) && is.list(X)) #TODO: check, names at level 1 = '.id'
        X = plyr::ldply(X, data.frame)
#TODO: convert nested list to data.frame first as well?
    dep_vars = all.vars(formula[[2]])
    indep_vars = all.vars(formula[[3]])

    form = as.formula(paste(indep_vars, collapse = "~"))
    res = sapply(dep_vars, function(v) reshape2::acast(
        as.data.frame(X), formula=form, value.var=v,
        fill=fill, fun.aggregate=fun.aggregate, ...
    ), simplify=FALSE)
    if (length(res) == 1) #TODO: drop_list in base?
        res[[1]]
    else
        res
}

#' Function to melt data.frame from one or multiple arrays
#'
#' @param ...       Array[s] or data.frame[s] to be melted
#' @param dimnames  List of names along the dimensions (instead of `VarX`)
#' @param na_rm     Remove rows with NAs
melt = function(..., dimnames=NULL, na_rm=TRUE) {
    l. = list(...)
    for (i in seq_along(l.)) {
        if (!is.null(dimnames))
            names(dimnames(l.[[i]])) = dimnames[1:length(dim(l.[[i]]))]
        l.[[i]] = reshape2::melt(l.[[i]], value.name=names(l.)[i], na.rm=na_rm)
    }

    Reduce(function(a,b) merge(a,b,all=!na_rm), l.)
}

#' Subsets an array using a list with indices or names
#'
#' @param X      The array to subset
#' @param index  A list of vector to use for subsetting
#' @param along  Along which dimension to subset if index is a vector; default is last dimension
#' @return       The subset of the array
subset = function(X, index, along=NULL, drop=FALSE) {
    if (is.list(index))
        abind::asub(X, index, drop=drop)
    else {
        if (is.null(along))
            along = length(dim(X))
        plyr::take(X, along=along, indices=index, drop=drop)
    }
}

#' Apply function that preserves order of dimensions
#'
#' @param X        An n-dimensional array
#' @param along    Along which axis to apply the function
#' @param FUN      A function that maps a vector to the same length or a scalar
map_simple = function(X, along, FUN) { #TODO: replace this by alply?
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
        if (length(dim(Y)) < length(dim(X)))
            Y
        else
            aperm(Y, c(along, preserveAxes))
    }
}

#' Maps a function along an array preserving its structure
#'
#' @param X        An n-dimensional array
#' @param along    Along which axis to apply the function
#' @param FUN      A function that maps a vector to the same length or a scalar
#' @param subsets  Whether to apply \code{FUN} along the whole axis or subsets thereof
#' @return         An array where \code{FUN} has been applied
map = function(X, along, FUN, subsets=rep(1,dim(X)[along])) {
    .check$all(X, along, subsets, x.to.array=TRUE)

    subsets = as.factor(subsets)
    lsubsets = as.character(unique(subsets)) # levels(subsets) changes order!
    nsubsets = length(lsubsets)

    # create a list to index X with each subset
    subsetIndices = rep(list(rep(list(TRUE), length(dim(X)))), nsubsets)
    for (i in 1:nsubsets)
        subsetIndices[[i]][[along]] = (subsets==lsubsets[i])

    # for each subset, call mymap
    resultList = lapply(subsetIndices, function(f)
        map_simple(subset(X, f), along, FUN))
#    resultList = lapply(subsetIndices, function(x) alply(subset(X, f), along, FUN)) FIXME:

    # assemble results together
    Y = do.call(function(...) abind::abind(..., along=along), resultList)
    if (dim(Y)[along] == nsubsets)
        base::dimnames(Y)[[along]] = lsubsets
    else if (dim(Y)[along] == dim(X)[along])
        base::dimnames(Y)[[along]] = base::dimnames(X)[[along]]
    drop(Y)
}

#' Splits and array along a given axis, either totally or only subsets
#'
#' @param X        An array that should be split
#' @param along    Along which axis to split
#' @param subsets  Whether to split each element or keep some together
#' @return         A list of arrays that combined make up the input array
split = function(X, along, subsets=c(1:dim(X)[along]), drop=FALSE) {
    if (!is.array(X) && !is.vector(X) && !is.data.frame(X))
        stop("X needs to be either vector, array or data.frame")
#    .check$all(X, along, subsets, x.to.array=FALSE) # breaks data.frame

    usubsets = unique(subsets)
    lus = length(usubsets)
    idxList = rep(list(rep(list(TRUE), length(dim(X)))), lus)

    for (i in 1:lus)
        idxList[[i]][[along]] = subsets==usubsets[i]

    if (length(usubsets)!=dim(X)[along] || !is.numeric(subsets))
        lnames = usubsets
    else
        lnames = base::dimnames(X)[[along]]
    setNames(lapply(idxList, function(ll) subset(X, ll, drop=drop)), lnames)
}

#' Intersects all passed arrays along a give dimension, and modifies them in place
#'
#' @param ...    Arrays that should be intersected
#' @param along  The axis along which to intersect
#' @param data   A list or environment to act upon
#TODO: accept along=c(1,2,1,1...) [maybe list w/ vectors as well?]
intersect = function(..., along=1, data=parent.frame()) {
    l. = list(...)
    varnames = match.call(expand.dots=FALSE)$...
    namesalong = lapply(l., function(f) dimnames(as.array(f))[[along]])
    common = do.call(.b$intersect, namesalong)
    for (i in seq_along(l.)) {
        dims = as.list(rep(T, length(dim(l.[[i]]))))
        dims[[along]] = common
        assign(as.character(varnames[[i]]),
               value = abind::asub(l.[[i]], dims),
               envir = data)
    }
}

#' Converts a list of character vectors to a logical matrix
#'
#' @param x  A list of character vectors
#' @return   A logical occurrence matrix
mask = function(x) {
    if (is.factor(x))
        x = as.character(x)

    vectorList = lapply(x, function(xi) setNames(rep(T, length(xi)), xi))
    t(stack(vectorList, fill=FALSE))
}

#' Summarize a matrix analogous to a grouped df in dplyr
#'
#' @param x      A matrix
#' @param from   Names that match the dimension `along`
#' @param to     Names that this dimension should be summarized to
#' @param along  Along which axis to summarize
#' @param FUN    Which function to apply, default is `mean`
#' @return       A summarized matrix as defined by `from`, `to`
summarize = function(x, to, from=rownames(x), along=1, FUN=aggr_error) {
    if (!is.matrix(x))
        stop('currently only matrices supported')
    if (along!=1)
        stop('currently only rows supported')

    lookup = .b$match(rownames(x), from, to, na_rm=TRUE)
    x = x[names(lookup),]
    
    # aggregate the rest using fun
    split(x, along=along, subsets=lookup) %>%
        lapply(function(x) map(x, along, FUN)) %>%
        do.call(rbind, .)
}
