context("lambda")

test_that("simple mult", {
    fx = function(x) x*2
    x = array(1:10)

    re = lambda(~ fx(x), along=c(x=1))
    expect_equal(re, c(x*2))
})

test_that("matrix mult", {
    dot =  function(x, y) sum(x * y)
    a = matrix(1:6, ncol=2)
    b = t(a)

    re = lambda(~ dot(a, b), along=c(a=1, b=2))
    dimnames(re) = NULL #FIXME:
    expect_equal(re, a %*% b)
})
