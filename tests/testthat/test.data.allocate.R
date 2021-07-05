test_that("test.data.allocate.inputs", {
    expect_error(allocateTreatments(trts = 4, idCol = "}"))
    expect_error(allocateTreatments(trts = 4, trtCol = "}"))
    expect_error(allocateTreatments(trts = 4, subjects = -1))
    expect_error(allocateTreatments(trts = 4, subjects = 10,
        prop = ".3,.7"))
    expect_error(allocateTreatments(trts = 4, subjects = 10,
        prop = ".3,.3,.1,2"))
    expect_error(allocateTreatments(trts = 4, subjects = c(10,
        10), prop = ".3,.3,.1,.3"))
})

test_that("test.data.allocate.names", {
    expect_error(allocateTreatments(trts = 6, subjects = 100,
        trtCol = "XX", idCol = "XX"))
})

test_that("test.data.allocate.ordered", {
    al <- allocateTreatments(trts = 4, subjects = 10, prop = ".3,.3,.1,.3",
        ordered = TRUE)
    expect_false(is.unsorted(al$TRT))
    al <- allocateTreatments(trts = 4, subjects = c(2, 2, 3,
        4), ordered = TRUE)
    expect_false(is.unsorted(al$TRT))
    expect_true(all(table(al[, 2]) == c(2, 2, 3, 4)))
    al <- allocateTreatments(trts = 4, subjects = c(2, 2, 3,
        4), ordered = FALSE)
    expect_true(all(table(al[, 2]) == c(2, 2, 3, 4)))
    expect_true(all(names(al) == c("SUBJ", "TRT")))
    al <- allocateTreatments(trts = 4, subjects = c(2, 2, 3,
        4), trtCol = "tr", idCol = "ID")
    expect_true(all(names(al) == c("ID", "tr")))
})
#
# test_that("test.data.allocate.repeatedTreatments", {
#     generateData(5, 20, treatSubj = c(3, 4, 3), treatDoses = c(0,
#         15, 30), respEqn = "DOSE")
#     x <- readAllData()
#     expect_true(all(sapply(split(x$TRT, x$Replicate), function(x) all(x[1:10] ==
#         x[11:20]))))
#     expect_false(all(x$TRT[x$Replicate == 1] == x$TRT[x$Replicate ==
#         2]))
#     generateData(5, 20, treatSubj = c(3, 4, 3), treatDoses = c(0,
#         15, 30), respEqn = "DOSE", treatDiff = FALSE)
#     x <- readAllData()
#     expect_true(all(sapply(split(x$TRT, x$Replicate), function(x) all(x[1:10] ==
#         x[11:20]))))
#     expect_true(all(x$TRT[x$Replicate == 1] == x$TRT[x$Replicate ==
#         2]))
# })

test_that("test.data.allocate.vectorOfVals", {
    al <- allocateTreatments(trts = 1:2, subjects = 10)
    expect_true(all(1:2 %in% al$TRT))
})

