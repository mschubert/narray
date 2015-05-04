.u = import('./util')

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
        dn = lapply(arrayList, .u$dimnames)
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
    for (i in .u$dimnames(arrayList, null.as.integer=T)) {
        dm = .u$dimnames(arrayList[[i]], null.as.integer=T)
        if (any(is.na(unlist(dm))))
            stop("NA found in array names, do not know how to stack those")
        if (newAxis)
            dm[[along]] = i
        result = do.call("[<-", c(list(result), dm, list(arrayList[[i]])))
    }
    result
}

if (is.null(module_name())) {
    A = matrix(1:4, nrow=2, ncol=2, dimnames=list(c('a','b'),c('x','y')))
    B = matrix(5:6, nrow=2, ncol=1, dimnames=list(c('b','a'),'z'))

    C = stack(list(A, B), along=2)
    #    x y z
    #  a 1 3 6   # B is stacked correctly according to its names
    #  b 2 4 5
    Cref = structure(c(1L, 2L, 3L, 4L, 6L, 5L), .Dim = 2:3,
                     .Dimnames = list(  c("a", "b"), c("x", "y", "z")))
    testthat::expect_equal(C, Cref)

    D = stack(list(m=A, n=C), along=3)
    # , , m          , , n
    #
    #   x y  z         x y z
    # a 1 3 NA       a 1 3 6
    # b 2 4 NA       b 2 4 5
    Dref = structure(c(1L, 2L, 3L, 4L, NA, NA, 1L, 2L, 3L, 4L, 6L, 5L),
                     .Dim = c(2L,3L, 2L), .Dimnames = list(c("a", "b"),
                     c("x", "y", "z"), c("m", "n")))
    testthat::expect_equal(D, Dref)
}
