context("bind")

test_that("vector", {
    ref = rbind(1:2, 3:4)
    expect_equal(ref, bind(list(1:2, 3:4), along=1))
    expect_equal(ref, t(bind(list(1:2, 3:4), along=2)))
})

test_that("keep names", {
    x = setNames(1:2, letters[1:2])
    m = bind(list(x,x), along=2)

    expect_equal(names(x), rownames(m))

    colnames(m) = LETTERS[1:2]
    expect_equal(bind(list(m,m), along=1), rbind(m, m))

    expect_equal(bind(list(m,m), along=2), cbind(m, m))
})
