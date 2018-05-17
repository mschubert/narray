context("map")

D = structure(c(1L, 2L, 3L, 4L, NA, NA, 1L, 2L, 3L, 4L, 6L, 5L),
              .Dim = c(2L, 3L, 2L), .Dimnames = list(c("a", "b"),
              c("x", "y", "z"), c("m", "n")))
x = D[,,"m"]

test_that("summarizing elements", {
    X = map(D, along=1, function(x) sum(x, na.rm=TRUE))
    #    m  n
    #  x 3  3
    #  y 7  7
    #  z 0 11
    Xref = structure(c(3L, 7L, 0L, 3L, 7L, 11L), .Dim = c(3L, 2L),
                     .Dimnames = list(c("x", "y", "z"), c("m", "n")))
    expect_equal(X, Xref)

    X3 = map(D, along=3, sum)
    #    x y  z
    #  a 2 6 NA
    #  b 4 8 NA
    X3ref = structure(c(2L, 4L, 6L, 8L, NA, NA), .Dim = 2:3, .Dimnames = list(
                      c("a", "b"), c("x", "y", "z")))
    expect_equal(X3, X3ref)

    l = letters
    id = function(x) x
    a = array(1:(2*3*4*5),
              dim = c(2,3,4,5),
              dimnames = list(l[1:2], l[3:5], l[6:9], l[10:14]))

    expect_equal(dim(a), dim(map_one(a, along=1, id)))
    expect_equal(dim(a), dim(map_one(a, along=2, id)))
    expect_equal(dim(a), dim(map_one(a, along=3, id)))
    expect_equal(dim(a), dim(map_one(a, along=4, id)))
    expect_equal(dim(a), dim(map(a, along=1, id)))
    expect_equal(dim(a), dim(map(a, along=2, id)))
    expect_equal(dim(a), dim(map(a, along=3, id)))
    expect_equal(dim(a), dim(map(a, along=4, id)))

    fx = function(x) sum(x)

    expect_equal(c(1,3,4,5), dim(map_one(a, along=1, fx, drop=FALSE)))
    expect_equal(c(1,3,4,5), dim(map(a, along=1, fx, drop=FALSE)))

    expect_equal(c(2,1,4,5), dim(map_one(a, along=2, fx, drop=FALSE)))
    expect_equal(c(2,1,4,5), dim(map(a, along=2, fx, drop=FALSE)))

    expect_equal(c(2,3,1,5), dim(map_one(a, along=3, fx, drop=FALSE)))
    expect_equal(c(2,3,1,5), dim(map(a, along=3, fx, drop=FALSE)))

    expect_equal(c(2,3,4,1), dim(map_one(a, along=4, fx, drop=FALSE)))
    expect_equal(c(2,3,4,1), dim(map(a, along=4, fx, drop=FALSE)))

    m = a[,,1,1] # 2 3
    expect_equal(c(1,3), dim(map_one(m, along=1, fx, drop=FALSE)))
    expect_equal(c(1,3), dim(map(m, 1, fx, drop=FALSE)))

    expect_equal(c(2,1), dim(map_one(m, along=2, fx, drop=FALSE)))
    expect_equal(c(2,1), dim(map(m, 2, fx, drop=FALSE)))
})

test_that("subset length must be same as array dimension", {
    expect_error(map(x, along=2, sum, subsets=c(1,1)))
})

test_that("rename as subsets only if they change length", {
    re = map(x, along=2, function(x) x*2, subsets=c(1,2,3))
    expect_equal(x*2, re)
})

test_that("NA subsets are dropped with a warning", {
    expect_warning(re <- map(x, along=2, sum, subsets=c(1,2,NA)))
    expect_equal(re, x[,1:2])
})
