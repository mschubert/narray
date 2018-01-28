context("construct")

DF = data.frame(expand.grid(x1=LETTERS[1:3], x2=LETTERS[4:5])[-3,], value=1:5,
                stringsAsFactors=FALSE)

Gref = structure(c(1L, 2L, NA, 3L, 4L, 5L), .Dim = c(3L, 2L),
                 .Dimnames = list(x1=c("A", "B", "C"), x2=c("D", "E")))

test_that("summing elements from data.frame to array", {
    G = construct(value ~ x1 + x2, data=DF)
    G2 = construct(DF, value ~ x1 + x2)
    #   D E
    # A 1 3
    # B 2 4
    # C 0 5

    expect_equal(G, Gref)
    expect_equal(G2, Gref)
})

test_that("axis variable is NA, should be omitted + print warning", {
    DFna = rbind(DF, NA)
    expect_warning(Gna <- construct(value ~ x1 + x2, data=DFna))
    expect_warning(Gna2 <- construct(DFna, value ~ x1 + x2))
    expect_equal(Gna, Gref)
    expect_equal(Gna2, Gref)
})

test_that("ambiguous row", {
    DFa = rbind(DF, c("A","D",6))
    expect_error(construct(value ~ x1 + x2, data=DFa))
    expect_error(construct(DFa, value ~ x1 + x2))
})

test_that("only one dependent variable", {
    expect_error(construct(value + other ~ x1 + x2, data=DF))
})
