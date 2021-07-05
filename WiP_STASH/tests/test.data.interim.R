test_that("test.data.interim.1", {
    expect_error(createInterims(10, method = "a"))
    expect_error(createInterims(-10))
    expect_error(createInterims(5, proportion = ".1,1.2"))
    expect_error(createInterims(5, proportion = ".4,.3"))
})

test_that("test.data.interim.2", {
    a <- createInterims(10, proportion = ".1,.3,.6")
    expect_equal(10, nrow(a))
    expect_equal(c("SUBJ", "INTERIM"), names(a))
    expect_equal(1:10, a[, 1])
    expect_error(createInterims(10, proportion = ".1,.3,.6", 
        idCol = ".1"))
    expect_error(createInterims(10, proportion = ".1,.3,.6", 
        interimCol = ".1"))
    expect_error(createInterims(10, proportion = ".1,.3,.6", 
        interimCol = "XX", idCol = "XX"))
    expect_equal("IDCOL", names(createInterims(10, proportion = ".1,.3,.6", 
        idCol = "IDCOL"))[1])
})

test_that("test.data.interim.3", {
    b <- createInterims(1000, proportion = ".1", method = "Sample")
    expect_equal(1:2, sort(unique(b[, 2])))
    expect_true(binom.test(table(b[, 2]), n = 1, p = 0.1)$p.value > 
        0.05)
})

test_that("test.data.interim.5", {
    a <- createInterims(10)
    expect_true(all(a[, 2] == 1))
})

