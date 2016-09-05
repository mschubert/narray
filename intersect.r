.b = import_('../base')
.s = import_('./subset')
.dn = import('./dimnames')

#' Intersects all passed arrays along a give dimension, and modifies them in place
#'
#' TODO: accept along=c(1,2,1,1...) [maybe list w/ vectors as well?]
#' TODO: accept data=env/list arg? [sig-comb/drug-tissue/assocs.r#62-65]
#'
#' @param ...    Arrays that should be intersected
#' @param along  The axis along which to intersect
#' @param envir  A list or environment to act upon
intersect = function(..., along=1, envir=parent.frame(), drop=FALSE) {
    dots = import_package_('pryr')$named_dots(...)

    # for `data.frame`s, replace the rownames by field that is referenced
    for (i in seq_along(dots)) {
        if (is.call(dots[[i]])) {
            if (along == 1 && is.data.frame(eval(dots[[i]][[2]], envir=envir))) {
                # as.data.frame: need rownames, not dplyr's
                df = as.data.frame(eval(dots[[i]][[2]], envir=envir)) 
                field = eval(dots[[i]], envir=envir)
                df$.rownames = rownames(df)
                rownames(df) = field
                names(dots)[i] = as.character(dots[[i]][[2]])
                dots[[i]] = df
            } else
                stop("calls can only reference `data.frame` fields with along=1")
        } else
            dots[[i]] = eval(dots[[i]], envir=envir)
    }

    dots = intersect_list(dots, along=along, drop=drop)

    # recover original rownames if we stored them separately
    for (name in names(dots))
        if (is.data.frame(dots[[name]]) && !is.null(dots[[name]]$.rownames)) {
            rownames(dots[[name]]) = dots[[name]]$.rownames
            dots[[name]]$.rownames = NULL
        }

    # modify the list or environment with the intersected results
    if (is.list(envir))
        assign(as.character(match.call()$envir),
               modifyList(envir, dots), envir=parent.frame())
    else
        for (name in names(dots))
            assign(name, dots[[name]], envir=envir)
}

intersect_list = function(l., along=1, drop=FALSE) {
    if (!is.list(l.))
        stop("`intersect_list()` expects a list as first argument, found: ", class(l.))

    common = do.call(.b$intersect, .dn$dimnames(l., along=along))
    lapply(l., function(e) .s$subset(e, index=common, along=along, drop=drop))
}

if (is.null(module_name())) {
    library(testthat)

    A = matrix(1:4, nrow=2, ncol=2, dimnames=list(c('a','b'),c('x','y')))
    C = structure(c(1L, 2L, 3L, 4L, 6L, 5L), .Dim = 2:3,
        .Dimnames = list(c("a", "b"), c("x", "y", "z")))
    E = C[,c(2,3,1)]
    DF = structure(list(y=3:4, z=c(6,5), x=1:2, A=c("b", "a")),
                   .Names=c("y","z","x","A"), row.names=1:2, class="data.frame")
    DF2 = as.data.frame(E[1,,drop=FALSE])

    AElist = intersect_list(list(A=A, E=E), along=2)

    # provided env should not change global vars or non-ref'd list elms
    ll = list(A=A, E=E, DF=DF)
    AEenv = intersect(A, E, along=2, envir=ll)
    expect_equal(ll$A, A)
    expect_equal(ll$E, AElist$E)
    expect_equal(ll$DF, DF)

    intersect(A, E, along=2)
    # > A         > E
    #   x y         x y   # along dimension 2, all arrays have same extent
    # a 1 3       a 1 3   # and same order of names; this function modifies
    # b 2 4       b 2 4   # values in-place
    
    AEref = structure(1:4, .Dim = c(2L, 2L),
                      .Dimnames = list(c("a", "b"), c("x", "y")))
    expect_equal(A, AEref)
    expect_equal(E, AEref)
    expect_equal(AElist$A, AEref)
    expect_equal(AElist$E, AEref)

    ADFlist = intersect_list(list(A=A, DF=DF2))
    expect_equal(ADFlist$A, AEref[1,,drop=FALSE])
    expect_equal(ADFlist$DF, DF2)

    ADFlist = intersect_list(list(A=A, DF=DF2), along=2, drop=TRUE)
    intersect(A, DF2, along=2, drop=TRUE)
    expect_equal(A, AEref)
    expect_equal(DF2, list(x=1, y=3))
    expect_equal(ADFlist$A, AEref)
    expect_equal(ADFlist$DF, DF2)

    DFref = DF[c(2,1),]
    rownames(DFref) = as.character(rownames(DFref)) # why, R?
    intersect(A, DF$A, along=1)
    expect_is(A, "matrix")
    expect_is(DF, "data.frame")
    expect_equal(DF, DFref)
    expect_true(all(DF == DFref))

    ll = list(a=setNames(1:5, letters[1:5]), b=setNames(2:4, letters[2:4]))
    lli = intersect_list(ll)
    intersect(a,b,envir=ll)
    expect_equal(ll$a, lli$a)
    expect_equal(ll$b, lli$b)
}
