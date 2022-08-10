context("stack")

A = matrix(1:4, nrow=2, ncol=2, dimnames=list(c('a','b'),c('x','y')))
B = matrix(5:6, nrow=2, ncol=1, dimnames=list(c('b','a'),'z'))
C = structure(c(1L, 2L, 3L, 4L, 6L, 5L), .Dim = 2:3,
              .Dimnames = list(c("a", "b"), c("x", "y", "z")))

test_that("match names, not extent", {
    Cref = stack(list(A, B), along=2)
    expect_equal(C, Cref)
})

test_that("supply objects instead of list", {
    Cobj = stack(A, B, along=2)
    expect_equal(Cobj, C)

    Cobj2 = stack(b=B, a=A, along=2)
    expect_equal(Cobj2, C[c(2,1),c(3,1,2)])
})

test_that("stack arrays with different dimensions", {
    B2 = as.array(drop(B))
    expect_error(stack(A, B2, along=1)) # no name in B2 along dim 2
    expect_error(stack(A, B2, along=3)) # no name in B2 along dim 2
    res = stack(A, z=B2, along=2)
    expect_equal(res, C)
})

test_that("fill empty elements", {
    D = stack(list(m=A, n=C), along=3)
    D2 = stack(m=A, n=C, along=3)
    # , , m          , , n
    #
    #   x y  z         x y z
    # a 1 3 NA       a 1 3 6
    # b 2 4 NA       b 2 4 5
    Dref = structure(c(1L, 2L, 3L, 4L, NA, NA, 1L, 2L, 3L, 4L, 6L, 5L),
                     .Dim = c(2L,3L, 2L), .Dimnames = list(c("a", "b"),
                     c("x", "y", "z"), c("m", "n")))
    expect_equal(D, Dref)
    expect_equal(D2, Dref)
})

test_that("same as first but without colnames", {
    colnames(A) = NULL
    colnames(B) = NULL
    colnames(C) = NULL
    Cnull = stack(list(A=A, B=B), along=2)
    expect_equal(C, Cnull)
})

test_that("vector stacking", {
    a = b = setNames(1:5, LETTERS[1:5])
    expect_equal(stack(list(a=a, b=b), along=1),
                 t(stack(list(a=a, b=b), along=2)))

    amat = as.matrix(a)
    colnames(amat) = "a"
    expect_equal(stack(list(a=a), along=2), amat)
    expect_equal(stack(list(a=a), along=1), t(amat))
})

test_that("1-row/col matrix stacking", {
    expect_equal(stack(B, along=1), B)
    expect_equal(stack(B, along=2), B)
    expect_equal(stack(list(B=B), along=1), B)
    expect_equal(stack(list(B=B), along=2), B)

    expect_equal(stack(t(B), along=1), t(B))
    expect_equal(stack(t(B), along=2), t(B))
    expect_equal(stack(list(B=t(B)), along=1), t(B))
    expect_equal(stack(list(B=t(B)), along=2), t(B))
})

test_that("allow overwrite", {
    ov = A
    ov[1,1] = 10
    expect_error(stack(ov, A, along=2))
    expect_error(stack(A, ov, along=2))
    expect_equal(stack(ov, A, along=2, allow_overwrite=TRUE), A)
    expect_equal(stack(A, ov, along=2, allow_overwrite=TRUE), ov)
    expect_equal(stack(A, A, along=2), A)

    ov[] = NA
    expect_equal(stack(A, ov, along=2), A)
    expect_equal(stack(ov, A, along=2), A)
    ov[] = 0
    expect_equal(stack(A, ov, along=2, fill=0), A)
    expect_equal(stack(ov, A, along=2, fill=0), A)
})

test_that("keep_empty arg when stacking zero-length vectors", {
    a = setNames(1:3, letters[1:3])
    b = numeric()
    re1 = stack(list(a=a, b=b), along=2, keep_empty=TRUE)
    expect_equal(re1,
          structure(c(1, 2, 3, NA, NA, NA), .Dim = c(3L, 2L),
                    .Dimnames = list(c("a", "b", "c"), c("a", "b"))))

    re2 = stack(list(a, b), along=1, keep_empty=TRUE)
    re3 = stack(list(a, b), along=2, keep_empty=TRUE)
    expect_equal(re3, structure(c(1, 2, 3, NA, NA, NA), .Dim = c(3L, 2L),
                    .Dimnames = list(c("a", "b", "c"), NULL)))
    expect_equal(re2, t(re3))
})

test_that("performance", {
    skip_on_cran()

    size = 500 # 500x500, 500 arrays
    syms = c(letters, LETTERS, 0:9)
    idx = do.call(paste0, expand.grid(syms, syms))

    ars = replicate(size, simplify=FALSE,
                    matrix(runif(size*size), nrow=size, ncol=size,
                           dimnames=list(sample(idx, size), sample(idx, size))))
    tt = system.time(stack(ars, along=2, allow_overwrite=TRUE))
    expect_lt(tt["user.self"], 6)
})
