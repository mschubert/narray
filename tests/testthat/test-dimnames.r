context("dimnames")

dn = list(c('a','b'),c('x','y'))
a = setNames(1:2, dn[[1]])
A = matrix(1:4, nrow=2, ncol=2, dimnames=dn)
DF = structure(list(y=3:4, z=c(6,5), x=1:2, A=c("b", "a")),
        .Names=c("y","z","x","A"), row.names=1:2, class="data.frame")
ll = list(a=a, A=A, DF=DF)

test_that("vector and dropping rules", {
    expect_equal(dimnames(a), dimnames(a, drop=FALSE))
    expect_equal(dimnames(a), dn[1])
    expect_equal(dimnames(a, drop=TRUE), dn[[1]])
    expect_equal(dimnames(a, along=1), dn[[1]])
    expect_equal(dimnames(a, along=1, drop=FALSE), dn[1])
})

test_that("ignore along for vector, not array", {
    expect_equal(dimnames(a, along=2), dn[[1]])
    expect_error(dimnames(as.array(a), along=2))
})

test_that("matrix", {
    expect_equal(dimnames(A), dn)
    expect_equal(dimnames(A, along=2), dn[[2]])
    expect_error(dimnames(A, along=3))
})

test_that("data.frame", {
    expect_equal(dimnames(DF, along=1), as.character(1:2))
})

test_that("list", {
    dnl = dimnames(ll)
    expect_equal(dnl$a, dn[1])
    expect_equal(dnl$A, dn)
    dnl1 = dimnames(ll, along=1)
    expect_equal(dnl1$a, dnl1$A, dn[[1]])
})

test_that("zero-length", {
    expect_equal(dimnames(c()), list(NULL)) #TODO: this should give integer()
    expect_equal(dimnames(c(), null_as_integer=TRUE), list(integer()))

    expect_null(dimnames(c(), drop=TRUE))
    expect_equal(dimnames(c(), null_as_integer=TRUE, drop=TRUE), integer())
})
