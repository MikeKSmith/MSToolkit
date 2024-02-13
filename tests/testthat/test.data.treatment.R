test_that("test.data.treatments.1", {
    expect_error(createTreatments(),
                 regexp = "Need arguments")
    expect_error(createTreatments(doses = c(0, 30),
                                  doseCol = "12invalid"),
                 regexp = "invalid R name")
    expect_error(createTreatments(doses = c(0, 30),
                                  trtCol = "+4353242invalid"),
                 regexp = "invalid R name")
    expect_error(createTreatments(doses = c(0, 30),
                                  timeCol = "~'lefw12invalid"),
                 regexp = "invalid R name")
    expect_error(createTreatments(doses = c(0, 30),
                                  times = c("a", "b")),
                 regexp = "Impossible to convert to numbers")
    expect_error(createTreatments(doses = c("da", "fes")),
                 regexp = "Impossible to convert to numbers")
})

test_that("test.data.treatments.crossover", {
    seqMat <- rbind(c(0, 0, 0),
                    c(0, 20, 10),
                    c(10, 10, 0),
                    c(20, 0, 20))
    expect_error(createTreatments(sequence = seqMat,
                                  times = c(1, 2)),
                regexp = "difference between the number of rows")
    expect_error(createTreatments(sequence = seqMat,
                                  times = c(-2, -1, 1, 2)),
                 regexp = "The sequence matrix suggests a dose run-in period")

    tr <- createTreatments(sequence = seqMat,
                           times = c(0, 1, 2, 3))

    expect_true(all(tr$DOSE[tr$TIME <= 0] == 0))
    for (i in 1:3) expect_true(all(tr$DOSE[tr$TRT == i] == seqMat[,i]))

    seqMat <- rbind(c("a", "b", "s"),
                    c("a", "a", "a"))

    expect_error(createTreatments(sequence = seqMat),
                 regexp = "must be a numeric matrix")

    x <- createTreatments(sequence = cbind(c(0, 15, 30),
                                           c(15,30, 0),
                                           c(30, 15, 0)))

    expect_true(identical(x,
                          data.frame(TRT = rep(1:3, each = 3),
                                     TIME = rep(1:3, 3),
                                     DOSE = c(0, 15, 30, 15, 30, 0, 30,15, 0))))

    x <- createTreatments(type = "c",
                          times = 0:3,
                          sequence = cbind(c(0,15, 30),
                                           c(15, 30, 0),
                                           c(30, 15, 0)))

    expect_true(identical(x,
                          data.frame(TRT = rep(1:3, each = 4),
                                     TIME = rep(0:3, 3),
                                     DOSE = c(0, 0, 15, 30,
                                              0, 15, 30,0,
                                              0, 30, 15, 0))))

    x <- createTreatments(doses = 1:2,
                          times = 0:1,
                          trtCol = "X",
                          timeCol = "Y",
                          doseCol = "Z")

    expect_true(identical(x,
                          data.frame(X = rep(1:2, each = 2),
                                     Y = rep(0:1, 2),
                                     Z = c(1, 1, 2, 2))))
})

test_that("test.data.treatments.parallel", {
    tr <- createTreatments(doses = c(0, 15, 30),
                           times = c(1, 2, 3))

    expect_true(is.data.frame(tr))
    expect_true(all(dim(tr) == c(9, 3)))
    expect_true(all(names(tr) == c("TRT", "TIME", "DOSE")))
    expect_true(all(sort(unique(tr$DOSE)) == c(0, 15, 30)))
    expect_true(all(tr$DOSE[tr$TRT == 1] == 0))
    expect_true(all(tr$DOSE[tr$TRT == 2] == 15))
    expect_true(all(tr$DOSE[tr$TRT == 3] == 30))

    tr <- createTreatments(doses = c(0, 15, 30),
                           times = c(-1, 0, 1, 2, 3))

    expect_true(all(tr$DOSE[tr$TIME < 0] == 0))
})

