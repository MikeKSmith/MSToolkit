test_that("test.data.checkDropuOutFun", {
    dFun <- function(data, prop) sample(0:1, nrow(data), TRUE, 
        c(1 - prop, prop))
    testData <- data.frame(SUBJ = rep(1:10, each = 5), TIME = rep(0:4, 
        10), VALUE = rnorm(50))
    doData <- createDropout(testData, dFun, prop = 0.4, seed = 10)
    expect_error(checkDropOutFun(max), info = "wrong function")
    expect_error(checkDropOutFun("ghost"), info = "does not exist")
    expect_error(checkDropOutFun(ghost), info = "does not exist")
    expect_error(checkDropOutFun(function(data, prop) 1:nrow(data), 
        testData), info = "wrong function, does not generate 0,1,T,F")
    expect_error(checkDropOutFun(function(data, prop) stop("error"), 
        doData), info = "error occuring")
    expect_true(checkDropOutFun(function(data, prop) rep(1, nrow(data)), 
        testData))
    expect_error(checkDropOutFun(function(dta, prop) rep(1, nrow(data)), 
        testData))
    dFun <- function(data, ...) rep(1, 5)
    expect_true(checkDropOutFun(dFun, testData))
    expect_error(checkDropOutFun(dFun, testData, useSubset = FALSE))
    expect_error(checkDropOutFun(dFun, testData, sizeSubset = 6))
})

test_that("test.data.missing.drop", {
    dFun <- function(data, prop) sample(0:1, nrow(data), TRUE, 
        c(1 - prop, prop))
    testData <- data.frame(SUBJ = rep(1:10, each = 5), TIME = rep(0:4, 
        10), VALUE = rnorm(50))
    doData <- createDropout(testData, dFun, prop = 0.4, seed = 10)
    out <- with(doData, tapply(MISSING, SUBJ, function(x) {
        w1 <- which(x == 1)
        length(w1) == 0 || all(w1 == seq(to = 5, length.out = length(w1)))
    }))
    expect_true(all(out), info = "check that the dropout is retained until the end")
    expect_error(createDropout(doData, max), info = "wrong function")
    expect_error(createDropout(doData, function(data, prop) 1:nrow(data)), 
        info = "wrong function, does not generate 0,1,T,F")
    expect_error(createDropout(doData, function(data, prop) stop("error")), 
        info = "error occuring")
    expect_error(createDropout(doData, "thisfunctiondoesnotexist"), 
        info = "function not existing")
    expect_error(createDropout(doData, dFun, idCol = "SUB"), 
        info = "idcol not in the dataset")
    expect_error(createDropout(doData, dFun, idCol = "634,4234-=ge5"), 
        info = "idcol is invalid")
    expect_error(createDropout(doData, dFun, idCol = "ID,SUB"), 
        info = "idcol must be of length one")
    expect_error(createDropout(doData, dFun, timeCol = "TT"), 
        info = "timecol not in the dataset")
    expect_error(createDropout(doData, dFun, timeCol = "634,4234-=ge5"), 
        info = "timecol is invalid")
    expect_error(createDropout(doData, dFun, timeCol = "TIME,time"), 
        info = "timecol must be of length one")
    expect_error(createDropout(doData, dFun, flagName = "634,42gr34-=ge5"), 
        info = "flagName is invalid")
    expect_error(createDropout(doData, dFun, flagName = "MISSING,MISS"), 
        info = "flagName must be of length one")
})

test_that("test.data.missing.mcar", {
    tdata <- data.frame(SUBJ = rep(1:3, each = 3), TIME = rep(0:2, 
        3))
    expect_true(all(createMCAR(tdata, prop = 0, flagName = "ABC")$ABC == 
        0))
    expect_true(all(createMCAR(tdata, prop = 1)$MISSING == 1))
    expect_error(createMCAR(tdata, prop = 100))
    expect_error(createMCAR(tdata, prop = -10))
    out <- createMCAR(tdata, prop = 1, rule = "TIME > 0")
    expect_true(all(out$MISSING[tdata$TIME == 0] == 0))
    expect_true(all(out$MISSING[tdata$TIME > 0] == 1))
    expect_error(createMCAR(tdata, prop = 1, rule = "NOTEXISTS > 0"))
    expect_error(createMCAR(tdata, prop = 1, rule = "TIME > 10"))
    expect_error(createMCAR(tdata, prop = 1, flagName = "0e321"))
    tdata <- expand.grid(SUBJ = 1:100, TIME = 0:4)
    tdata$MISSING <- sample(c(0, 1), size = 500, replace = TRUE)
    out <- createMCAR(tdata, prop = 0.5)
    expect_true(all(out$MISSING[tdata$MISSING == 1] == 1))
})

