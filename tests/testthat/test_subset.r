context("subset")

A = matrix(1:10, ncol=2)
b = 1:5
C = matrix(rnorm(20), dimnames=list(letters[1:5],LETTERS[1:4]), nrow=5, ncol=4)
DF = as.data.frame(lapply(as.data.frame(A), as.factor))

test_that("everything", {
    expect_equal(subset(A, c(1,2), along=1), A[1:2,])
    expect_equal(subset(A, c(1,2), along=2), A)

    expect_equal(subset(b, 1:5 %in% c(2,4,5), along=1), subset(A[,1], c(2,4,5)))
    expect_error(subset(b, 1, along=2))

    expect_equal(subset(DF, c(1,2), along=1), DF[1:2,])
    expect_equal(subset(DF, c(1,2), along=2), DF)

    expect_equal(C[,2:3],
                subset(C, 2:3, along=2, drop=TRUE),
                subset(C, 2:3, along=2, drop=FALSE),
                subset(C, LETTERS[2:3], along=2, drop=TRUE),
                subset(C, LETTERS[2:3], along=2, drop=FALSE),
                subset(C, c(FALSE, TRUE, TRUE, FALSE, drop=TRUE)),
                subset(C, c(FALSE, TRUE, TRUE, FALSE, drop=FALSE)))

    expect_equal(C[,3,drop=FALSE],
                subset(C, 3, along=2, drop=FALSE),
                subset(C, LETTERS[3], along=2, drop=FALSE),
                subset(C, c(FALSE, FALSE, TRUE, FALSE, along=2, drop=FALSE)))
})

test_that("subset with NA", {
    expect_error(subset(1:5, index=c(rep(FALSE, 4), NA)))
})
