context("rep")

test_that("vector", {
    ref = rbind(1:2, 1:2)
    expect_equal(ref, rep(1:2, 2, along=1))
    expect_equal(ref, t(rep(1:2, 2, along=2)))
    expect_equal(ref, rrep(1:2, 2))
    expect_equal(ref, t(crep(1:2, 2)))
})

test_that("keep names", {
    x = setNames(1:2, letters[1:2])
    m = rep(x, 2, along=2)

    expect_equal(names(x), rownames(m))

    colnames(m) = LETTERS[1:2]
    expect_equal(rbind(m, m), rep(m, 2, along=1))
    expect_equal(rbind(m, m), rrep(m, 2))

    expect_equal(cbind(m, m), rep(m, 2, along=2))
    expect_equal(cbind(m, m), crep(m, 2))
})
