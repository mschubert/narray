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
