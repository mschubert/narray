context("melt")

test_that("melt matrix", {
    A = matrix(1:4, nrow=2, ncol=2, dimnames=list(c('a','b'),c('x','y')))
    expect_equal(melt_one(A), melt(A))
})

test_that("keep object name if multiple", {
    A = matrix(1:4, nrow=2, ncol=2, dimnames=list(c('a','b'),c('x','y')))
    B = A
    res = melt(A, B)
    expect_equal(colnames(res), c("Var1", "Var2", "A", "B"))
    expect_equal(res$A, res$B)
})
