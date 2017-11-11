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
