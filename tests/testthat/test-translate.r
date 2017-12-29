context("translate")

test_that("mean of same names", {
    G = matrix(c(1,2,0,3,4,5), nrow=3,
               dimnames=list(c('A','B','C'), c('D','E')))

    W = translate(G, along=1, from=rownames(G), to=c('a','b','b'), FUN=mean)
    #   D   E
    # a 1 3.0
    # b 1 4.5

    Wref = structure(c(1, 1, 3, 4.5), .Dim = c(2L, 2L),
                     .Dimnames = list(c("a", "b"), c("D", "E")))
    expect_equal(W, Wref)
})
