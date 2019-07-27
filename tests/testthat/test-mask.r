context("mask")

test_that("mask", {
    F = list(a=c('e1','e2'),b='e1',c='e2')

    Z = mask(F)
    #      e1    e2
    # a  TRUE  TRUE
    # b  TRUE FALSE
    # c FALSE  TRUE

    Zref = structure(c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE), .Dim = c(3L,
    2L), .Dimnames = list(c("a", "b", "c"), c("e1", "e2")))

    expect_equal(Zref, Z)
    expect_equal(Zref, mask(F, along=2))
    expect_equal(Zref, t(mask(F, along=1)))
})

test_that("NA is dropped without error", {
    G = c("a", "b", NA, "a")
    Gm = mask(G)
    Gref = structure(c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE),
                     .Dim = c(4L, 2L), .Dimnames = list(NULL, c("a", "b")))

    expect_equal(Gm, Gref)
})

test_that("performance", {
    skip_on_cran()

    x = factor(sample(1e2, 5e3, replace=TRUE))
    tt = system.time(narray::mask(x, along=2))
    expect_lt(tt["user.self"], 5) #TODO: this should be faster (#26)
})
