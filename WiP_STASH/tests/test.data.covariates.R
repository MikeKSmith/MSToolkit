test_that("test.data.covariates.cont", {
    expect_error(createContinuousCovariates(10, mean = "a,b"))
    expect_error(createContinuousCovariates(-10, mean = "0,1"))
    expect_error(createContinuousCovariates(10, mean = "0,1", 
        covariance = "1,1,1,1,1,1"))
    expect_error(createContinuousCovariates(10, mean = "0,1", 
        names = "b"))
    expect_error(createContinuousCovariates(10, mean = "0,1", 
        names = c("X", "X")))
    expect_error(createContinuousCovariates(10, mean = "0,1", 
        names = c("X", ".23")))
    expect_error(createContinuousCovariates(10, mean = "0,1", 
        names = c("X", "Y"), digits = -1))
    expect_error(createContinuousCovariates(10, mean = "0,1", 
        names = c("X", "Y"), maxDraws = -100))
    expect_error(createContinuousCovariates(10, mean = "0,1", 
        names = c("X", "Y"), idCol = ".534"))
    dat <- createContinuousCovariates(10, mean = "0,1", names = c("X", 
        "Y"))
    expect_equal(10, nrow(dat))
    expect_equal(3, ncol(dat))
    expect_equal(c("SUBJ", "X", "Y"), names(dat))
})

test_that("test.data.covariates.disc", {
    dat <- createDiscreteCovariates(10, names = "X", probs = ".1,.9", 
        values = "1,2")
    expect_equal(10, nrow(dat))
    expect_true(all(dat[, 2] %in% 1:2))
    expect_error(createDiscreteCovariates(10, probs = ".1,.9", 
        values = "1,2", names = "43"))
    expect_error(createDiscreteCovariates(10, probs = ".1,.9", 
        values = "1,2", idCol = "43"))
    expect_error(createDiscreteCovariates(100, probs = ".1,.9#.3,.3,.4", 
        values = "1,2#1,3"))
    expect_error(createDiscreteCovariates(100, probs = ".1,.9#.3,4", 
        values = "1,2#1,3", names = c("F1", "F2")))
    expect_error(createDiscreteCovariates(100, probs = ".1,.9#1#1", 
        values = "1,2#1,3", names = c("F1", "F2")))
    pa <- data.frame(F1 = rep(0:1, 3), F2 = rep(1:3, each = 2), 
        PROB = c(0.1, 0.2, 0.1, 0.2, 0.2, 8))
    expect_error(createDiscreteCovariates(100, probArray = pa))
    padf <- data.frame(F1 = rep(0:1, 3), F2 = rep(1:3, each = 2), 
        PROB = c(0.1, 0.2, 0.1, 0.2, 0.2, 0.2))
    paArr <- rbind(c(0.1, 0.1, 0.2), c(0.2, 0.2, 0.2))
    outDf <- createDiscreteCovariates(100, probArray = padf, 
        seed = 10)
    outArr <- createDiscreteCovariates(100, values = list(0:1, 
        1:3), probArray = paArr, names = "F1,F2", seed = 10)
    expect_equal(outArr, outDf, info = "checking the prob array handling")
    expect_error(createDiscreteCovariates(5, names = "D1,D2", 
        values = "1,2#1,2,3", probArray = rbind(c(0.1, 0.1, 0.3), 
            c(0.3, 8, 0.1))), info = "check reject proba array as an array and does not sum up to one")
    out <- createDiscreteCovariates(5, names = "D1,D2", values = "1,2#1,2,3", 
        probArray = rbind(c(0, 0, 0), c(0, 0, 1)))
    expect_true(all(out$D1 == 2 & out$D3 == 3), info = "checking 0 probabilities")
})

test_that("test.data.covariates.disc.debug", {
    subjects = 10
    names = "X, Y, Z"
    values1 = "1,2#7,8,9#a,b"
    probs1 = ".1,.9#.5,.4,.1#.5,.5"
    values2 = c("1,2", "7,8,9", "a,b")
    probs2 = c(".1,.9", ".5,.4,.1", ".5,.5")
    values3 = list(c(1, 2), c(7, 8, 9), c("a", "b"))
    probs3 = list(c(0.1, 0.9), c(0.5, 0.4, 0.1), c(0.5, 0.5))
    pArray1 <- data.frame(expand.grid(X = 1:2, Y = 7:9, Z = c("a", 
        "b")), PROB = c(rep(0.08, 10), 0.1, 0.1))
    pArray2 <- array(c(rep(0.08, 10), 0.1, 0.1), dim = c(2, 3, 
        2))
    pArray3 <- array(c(rep(0.08, 10), 0.1, 0.1), dim = c(2, 3, 
        2), dimnames = list(c(1, 2), c(7, 8, 9), c("a", "b")))
    dat1 <- createDiscreteCovariates(subjects = subjects, names = names, 
        values = values1, probs = probs1)
    dat2 <- createDiscreteCovariates(subjects = subjects, names = names, 
        values = values2, probs = probs2)
    dat3 <- createDiscreteCovariates(subjects = subjects, names = names, 
        values = values3, probs = probs3)
    dat4 <- createDiscreteCovariates(10, probArray = pArray1)
    dat6 <- createDiscreteCovariates(10, names = "X, Y, Z", probArray = pArray3)
    expect_true(is.data.frame(dat4))
    expect_true(is.data.frame(dat6))
})

test_that("test.data.covariates.disc.handleProbArray", {
    values0 <- list(c(7, 8, 9), c("a", "b"))
    probs0 <- list(c(0.5, 0.4, 0.1), c(0.5, 0.5))
    names(values0) <- c("Y", "Z")
    names(probs0) <- c("Y", "Z")
    p0 <- c(0.25, 0.2, 0.05, 0.25, 0.2, 0.05)
    pArray1 <- data.frame(expand.grid(a = 7:9, b = c("a", "b")), 
        PROB = p0)
    pArray2 <- array(p0, dim = c(3, 2))
    pArray3 <- array(p0, dim = c(3, 2), dimnames = list(c(7, 
        8, 9), c("a", "b")))
    grid1 <- MSToolkit:::.handleProbArray(values = values0, probs = probs0)
    grid2 <- MSToolkit:::.handleProbArray(probArray = pArray1, 
        values = values0)
    grid3 <- MSToolkit:::.handleProbArray(probArray = pArray2, 
        values = values0)
    grid4 <- MSToolkit:::.handleProbArray(probArray = pArray3, 
        values = values0)
    expect_equal(grid3, grid1)
    expect_equal(grid4, grid1)
    values0 <- list(c(1, 2), c(7, 8, 9), c("a", "b"))
    probs0 <- list(c(0.1, 0.9), c(0.5, 0.4, 0.1), c(0.5, 0.5))
    names(values0) <- c("X", "Y", "Z")
    names(probs0) <- c("X", "Y", "Z")
    p0 <- c(0.025, 0.225, 0.02, 0.18, 0.005, 0.045, 0.025, 0.225, 
        0.02, 0.18, 0.005, 0.045)
    pArray1 <- data.frame(expand.grid(X = 1:2, Y = 7:9, Z = c("a", 
        "b")), PROB = p0)
    pArray2 <- array(p0, dim = c(2, 3, 2))
    pArray3 <- array(p0, dim = c(2, 3, 2), dimnames = list(c(1, 
        2), c(7, 8, 9), c("a", "b")))
    grid1 <- MSToolkit:::.handleProbArray(values = values0, probs = probs0)
    grid2 <- MSToolkit:::.handleProbArray(probArray = pArray1, 
        values = values0)
    grid3 <- MSToolkit:::.handleProbArray(probArray = pArray2, 
        values = values0)
    grid4 <- MSToolkit:::.handleProbArray(probArray = pArray3, 
        values = values0)
    expect_equal(grid3, grid1)
    expect_equal(grid4, grid1)
})

test_that("test.data.covariates.ext", {
    expect_error(createExternalCovariates(20, names = "X", file = "thisDoesNotExists.csv"), 
        info = "Unexisting file generates error")
    expect_error(createExternalCovariates(20, names = "X1,X2,X3", 
        file = "wrongTestCovariates.csv", workingPath = covariates.datapath), 
        info = "Not correctly formatted csv file generates error")
    testFile <- "testCovariates.csv"
    expect_error(createExternalCovariates(20, names = "YY", file = testFile, 
        workingPath = covariates.datapath), info = "Unfound variables in the file generates error")
    expect_error(createExternalCovariates(20, names = "X1", file = testFile, 
        dataId = "SUBJECTS", workingPath = covariates.datapath), 
        info = "Unfound `dataId` in the file generates error")
    expect_error(createExternalCovariates(20, names = "X1", file = testFile, 
        refCol = "SUBJECTS", workingPath = covariates.datapath), 
        info = "Unfound `refCol` in the file generates error")
    expect_error(createExternalCovariates(20, names = ".25352"), 
        info = "Invalid `names` generates an error")
    expect_error(createExternalCovariates(20, names = "X1,X1"), 
        info = "Duplicated `names` generates an error")
    expect_error(createExternalCovariates(20, names = "X1", dataId = ".43gt4e"), 
        info = "Wrong `dataId` generates an error")
    expect_error(createExternalCovariates(20, names = "X1", dataId = ".43gt4e"), 
        info = "Wrong `refCol` generates an error")
    expect_error(createExternalCovariates(20, names = "X1", idCol = ".43fewfgt4e"), 
        info = "Wrong `dataId` generates an error")
    expect_error(createExternalCovariates(20, names = "X1", subset = "1<X1<2<4"), 
        info = "Incorrect subset code (Too many comparators) generates error")
    expect_error(createExternalCovariates(20, names = "X1", subset = "X1"), 
        info = "Incorrect subset code (Too few comparators) generates error")
    expect_error(createExternalCovariates(20, names = "X1", subset = "-1202@{} > 1"), 
        info = "Incorrect subset code generates error")
    expect_error(createExternalCovariates(20, names = "X1", subset = "X1 >"), 
        info = "Incorrect subset code (Empty side) generates error")
    expect_error(createExternalCovariates(20, names = "X1", file = testFile, 
        subset = "YY > 4", dataId = "ID", workingPath = covariates.datapath), 
        info = "subset on unexisting variables generates an error")
    expect_error(createExternalCovariates(20, names = "X1", subset = "X1 > 100", 
        dataId = "ID", file = testFile, workingPath = covariates.datapath), 
        info = "percent must be lower than 100")
    expect_true(all(createExternalCovariates(20, names = "X1", 
        subset = "X1 > 0", file = testFile, dataId = "ID", workingPath = covariates.datapath)$X1 > 
        0), info = "subset correctly applied")
    dat <- createExternalCovariates(20, names = "X1", subset = ".7 < X1 < .8", 
        dataId = "ID", file = testFile, workingPath = covariates.datapath)$X1
    expect_true(all(dat > 0.7 & dat < 0.8), info = "subset correctly applied")
    dat <- createExternalCovariates(20, names = "X1", dataId = "ID", 
        subset = c(".7 < X1 < .8", "-1 <= X2 <= 1"), file = testFile, 
        workingPath = covariates.datapath)
    expect_true(all(dat$X1 > 0.7 & dat$X1 < 0.8 & dat$X2 >= -1 & 
        dat$X2 <= 1), info = "subset correctly applied")
    expect_error(createExternalCovariates(20, names = "X1", sameRow = FALSE, 
        refCol = "ID", dataId = "ID", file = testFile, workingPath = covariates.datapath), 
        info = "checking incompatibility between refCol and sameRow")
    expect_error(createExternalCovariates(20, names = "X1", dataId = "ID", 
        file = testFile, workingPath = covariates.datapath, percent = "x"), 
        info = "percent can't be converted to a number")
    expect_error(createExternalCovariates(20, names = "X1", dataId = "ID", 
        file = testFile, workingPath = covariates.datapath, percent = "10,20"), 
        info = "percent must be of length 1")
    expect_error(createExternalCovariates(20, names = "X1", dataId = "ID", 
        file = testFile, workingPath = covariates.datapath, percent = "-10"), 
        info = "percent must be greater than 0")
    expect_error(createExternalCovariates(20, names = "X1", dataId = "ID", 
        file = testFile, workingPath = covariates.datapath, percent = "1910"), 
        info = "percent must be lower than 100")
    testSameRowFile <- "testSameRow.csv"
    dataSameRow <- createExternalCovariates(20, names = "X,Y", 
        dataId = "ID", file = testSameRowFile, sameRow = TRUE, 
        workingPath = covariates.datapath)[, -1]
    expect_true(all(apply(dataSameRow, 1, diff) == 0), info = "checking the sameRow functionality")
    out <- createExternalCovariates(50, names = "X1,X2", dataId = "ID", 
        file = testFile, workingPath = covariates.datapath)
    expect_equal(50, nrow(out), info = "Checking the number of rows of the output")
    expect_true(all(names(out) %in% c("X1", "X2", "SUBJ")), info = "Checking the naming of columns")
    out <- createExternalCovariates(50, names = "X1,X2", file = testFile, 
        workingPath = covariates.datapath, idCol = "SUB", dataId = "ID")
    expect_true(all(names(out) %in% c("X1", "X2", "SUB")), info = "Checking the naming of columns")
    out <- createExternalCovariates(50, names = "X1,X2", file = testFile, 
        workingPath = covariates.datapath, idCol = "SUB", dataId = "ID", 
        refCol = "ID")
    expect_true(all(names(out) %in% c("X1", "X2", "SUB", "ID.refCol")), 
        info = "Checking the naming of columns")
    out <- createExternalCovariates(50, names = "X1,X2", file = testFile, 
        workingPath = covariates.datapath, idCol = "SUB", dataId = "ID", 
        refCol = "ID", refColSuffix = "")
    expect_true(all(names(out) %in% c("X1", "X2", "SUB", "ID.")), 
        info = "Checking the naming of columns")
    out1 <- createExternalCovariates(50, names = "X1,X2", dataId = "ID", 
        file = testFile, workingPath = covariates.datapath, seed = 10)
    rnorm(1002)
    out2 <- createExternalCovariates(50, names = "X1,X2", dataId = "ID", 
        file = testFile, workingPath = covariates.datapath, seed = 10)
    expect_true(identical(out1, out2))
})

test_that("test.data.covariates.sf3", {
    expect_error(createContinuousCovariates(10, mean = "100,100,100", 
        names = c("X", "Y", "Z"), digits = "2,3"), info = "digits should have the right length")
    expect_error(createContinuousCovariates(10, mean = "100,100,100", 
        names = c("X", "Y", "Z"), digits = "2,3,-2"), info = "no negative digits")
    out <- createContinuousCovariates(10, mean = "100,100,100", 
        names = c("X", "Y", "Z"), digits = "2,3,2")
    expect_equal(round(out[, 2], 2), out[, 2], info = "check the use of a digits vector")
    expect_equal(round(out[, 3], 3), out[, 3], info = "check the use of a digits vector (2)")
    expect_equal(round(out[, 4], 2), out[, 4], info = "check the use of a digits vector (3)")
    out <- createContinuousCovariates(10, mean = "100,100,100", 
        names = c("X", "Y", "Z"), digits = "3")
    expect_equal(round(out[, 2:4], 3), out[, 2:4], info = "check the use of a digits not vector")
})

test_that("test.data.covariates.timevarying", {
    subjects <- 1:10
    names <- "X, Y, Z"
    mean <- list(X = 1:4, Y = rep(3, 4), Z = "2.5, 3, 3.2, 3.6")
    covariance = list(1, 2:5, cbind(c(1, 0.5, 0.3, 0), c(0.5, 
        1, 0, 0), c(0.3, 0, 1, 0), c(0, 0, 0, 1)))
    range = list("10>=X>0", NULL, c("Z>0", "Z<=10"))
    digits = 2
    maxDraws = 100
    seed = 99
    idCol = "SUBJ"
    timeCol = "TIME"
    treatPeriod = c(0.25, 0.5, 1, 12)
    dat <- createTimeVaryingCovariates(10, "X, Y, Z", mean <- list(X = 1:4, 
        Y = rep(3, 4), Z = "2.5, 3, 3.2, 3.6"), covariance = list(1, 
        2:5, cbind(c(1, 0.5, 0.3, 0), c(0.5, 1, 0, 0), c(0.3, 
            0, 1, 0), c(0, 0, 0, 1))), range = list("10>=X>0", 
        NULL, c("Z>0", "Z<=10")), idCol = "SUBJ", timeCol = "TIME", 
        treatPeriod = c(0.25, 0.5, 1, 12))
    expect_equal(c(0.25, 0.5, 1, 12), unique(dat$TIME))
})

test_that("test.data.covariates.wrapper", {
    testFile <- "testCovariates.csv"
    expect_error(createCovariates(subjects = -3), info = "wrong subjects")
    expect_error(createCovariates(subjects = 10, idCol = "ID, SUB"), 
        info = "id too long")
    expect_error(createCovariates(subjects = 10, idCol = "4542"), 
        info = "invalid ID")
    expect_true(all(createCovariates(subjects = 100) == 1:100), 
        info = "test when no covariates")
    expect_equal("SUB", names(createCovariates(subjects = 100, 
        idCol = "SUB")), info = "test idCol")
    expect_error(createCovariates(30, conNames = "X1,X2", extNames = "X2, X3", 
        disNames = "X4, X5"), info = "incompatibility in names")
    d1 <- createCovariates(30, conNames = "X,Y", conMean = "0,0", 
        seed = 10)
    d2 <- createContinuousCovariates(30, names = "X,Y", mean = "0,0", 
        seed = 10)
    expect_equal(d2, d1, info = "simple check only continuous covariates")
    d1 <- createCovariates(30, conNames = "X,Y", conMean = "0,0", 
        conCov = "1,0,1", seed = 10)
    d2 <- createContinuousCovariates(30, names = "X,Y", mean = "0,0", 
        covariance = "1,0,1", seed = 10)
    expect_equal(d2, d1, info = "simple check only continuous covariates, using cov matrix")
    d1 <- createCovariates(30, conNames = "X,Y", conMean = "0,0", 
        conCov = "1,0,1", seed = 10, conRange = "-1<X<1")
    d2 <- createContinuousCovariates(30, names = "X,Y", mean = "0,0", 
        covariance = "1,0,1", seed = 10, range = "-1<X<1")
    expect_equal(d2, d1, info = "simple check only continuous covariates, with range")
    d1 <- createCovariates(70, disNames = "P1,P2", disValues = "1,2#3,5,6", 
        disProbs = ".5,.5#.3,.3,.4", seed = 10)
    d2 <- createDiscreteCovariates(70, names = "P1,P2", values = "1,2#3,5,6", 
        probs = ".5,.5#.3,.3,.4", seed = 10)
    expect_equal(d2, d1, info = "simple check only discrete covariates")
    d1 <- createExternalCovariates(80, names = "X1", dataId = "ID", 
        subset = c(".7 < X1 < .8", "-1 <= X2 <= 1"), seed = 3, 
        file = testFile, workingPath = covariates.datapath)
    d2 <- createCovariates(80, extNames = "X1", extSubset = c(".7 < X1 < .8", 
        "-1 <= X2 <= 1"), extFile = testFile, extDataId = "ID", 
        workingPath = covariates.datapath, seed = 3)
    expect_equal(d2, d1, info = "simple check only external covariates")
    dAll <- createCovariates(30, conNames = "X,Y", conMean = "0,0", 
        conCov = "1,0,1", seed = 10, conRange = "-1<X<1", disNames = "P1,P2", 
        disValues = "1,2#3,5,6", disProbs = ".5,.5#.3,.3,.4", 
        extNames = "X1", extDataId = "ID", extFile = testFile, 
        workingPath = covariates.datapath)
    dCon <- createContinuousCovariates(30, names = "X,Y", mean = "0,0", 
        covariance = "1,0,1", seed = 10, range = "-1<X<1")
    dDis <- createDiscreteCovariates(30, names = "P1,P2", values = "1,2#3,5,6", 
        probs = ".5,.5#.3,.3,.4", seed = 10)
    dExt <- createExternalCovariates(30, names = "X1", dataId = "ID", 
        file = testFile, workingPath = covariates.datapath, seed = 10)
    expect_true(identical(dAll[, c("SUBJ", "X", "Y")], dCon), 
        info = "check altogether 1")
    expect_true(identical(dAll[, c("SUBJ", "P1", "P2")], dDis), 
        info = "check altogether 2")
    expect_true(identical(dAll[, c("SUBJ", "X1")], dExt), info = "check altogether 3")
})

