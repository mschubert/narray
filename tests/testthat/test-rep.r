context("rep")

test_that("vector", {
    expect_equal(rep(1:2, 2, along=1),
                 t(rep(1:2, 2, along=2)),
                 rrep(1:2, 2),
                 t(crep(1:2, 2)),
                 cbind(1:2, 1:2))
})

test_that("keep names", {
    x = setNames(1:2, letters[1:2])
    m = rep(x, 2, along=2)

    expect_equal(names(x), rownames(m))

    colnames(m) = LETTERS[1:2]
    expect_equal(rep(m, 2, along=1),
                 rrep(m, 2),
                 rbind(m, m))

    expect_equal(rep(m, 2, along=2),
                 crep(m, 2),
                 cbind(m, m))
})
