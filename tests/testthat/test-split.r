context("split")

test_that("3-dimensional array", {
    C = structure(c(1L, 2L, 3L, 4L, 6L, 5L), .Dim = 2:3,
        .Dimnames = list(c("a", "b"), c("x", "y", "z")))

    Y = split(C, along=2, subsets=c('s1','s1','s2'))
    # $s1          $s2
    #   x y        a b   # each subset is split into a separate array
    # a 1 3        5 6
    # b 2 4

    Yref = structure(list(s1 = structure(1:4, .Dim = c(2L, 2L),
        .Dimnames = list(c("a", "b"), c("x", "y"))),
        s2 = structure(c(6L, 5L), .Dim = c(2L,1L),
        .Dimnames = list(c("a", "b"), "z"))), .Names = c("s1", "s2"))

    expect_equal(Y, Yref)
})

test_that("NA in subsets", {
    C = structure(c(1L, 2L, 3L, 4L, 6L, 5L), .Dim = 2:3,
        .Dimnames = list(c("a", "b"), c("x", "y", "z")))

    expect_warning(x <- split(C, along=2, subsets=c(1,1,NA)))
    expect_equal(x, list(`1`=C[,c(1:2)]))
})

test_that("drop if same dimensions", {
    C = structure(c(1L, 2L, 3L, 4L, 6L, 5L), .Dim = 2:3,
        .Dimnames = list(c("a", "b"), c("x", "y", "z")))

    expect_equal(split(C, along=2),
                 split(C, along=2, drop=TRUE))

    expect_equal(split(C, along=2, subsets=c(1,1,2)),
                 split(C, along=2, subsets=c(1,1,2), drop=FALSE))
})
