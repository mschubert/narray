context("melt")

test_that("melt matrix", {
    A = matrix(1:4, nrow=2, ncol=2, dimnames=list(c('a','b'),c('x','y')))
    expect_equal(melt_one(A), melt(A))
})
