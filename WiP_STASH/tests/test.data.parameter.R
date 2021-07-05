test_that("test.data.param.ext", {
    testParamFile <- "testParam.csv"
    otherFiles <- paste("testParam", c("nonmem", "spaceDelim", 
        "tabDelim"), sep = ".")
    wrongTestParamFile <- "wrongTestParam.csv"
    expect_error(createExternalParameters(-20), info = "`subjects` must be positive")
    expect_error(createExternalParameters(20, names = ".54325,5432"), 
        info = "invalid `names`")
    expect_error(createExternalParameters(20, names = "X,X"), 
        info = "duplicated `names`")
    expect_error(createExternalParameters(20, names = "X,Y", 
        idCol = "X"), info = "duplicated names between `SUBJ` and `names`")
    expect_error(createExternalParameters(20, names = "X", dataId = ".54325"), 
        info = "invalid `dataId`")
    expect_error(createExternalParameters(20, names = "X", dataId = "ID,SUB"), 
        info = "`dataId` should be of length one")
    expect_error(createExternalParameters(20, names = "X", idCol = ".54325"), 
        info = "invalid `idCol`")
    expect_error(createExternalParameters(20, names = "X", idCol = "ID,SUB"), 
        info = "`idCol` should be of length one")
    expect_error(createExternalParameters(20, names = "E0,ED50", 
        file = testParamFile, workingPath = parameters.datapath, 
        betNames = "B1,B1"), info = "Duplicated values in betNames")
    expect_error(createExternalParameters(20, names = "E0,ED50,EMAX", 
        file = "doesNotExists.csv", workingPath = parameters.datapath), 
        info = "File does not exist")
    expect_error(createExternalParameters(20, names = "E0,ED50,EMAX", 
        file = wrongTestParamFile, workingPath = parameters.datapath), 
        info = "wrong formatted file")
    expect_error(createExternalParameters(20, names = "X,Y,ED50", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath), 
        info = "missing variables in file")
    expect_error(createExternalParameters(20, names = "E0,ED50", 
        file = testParamFile, workingPath = parameters.datapath, 
        idCol = "SUB"), info = "missing ID variables in file")
    expect_error(createExternalParameters(20, names = "E0,ED50", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        errStruc = "w"), info = "wrong errStruc")
    dataNone <- createExternalParameters(20, names = "E0,ED50", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        errStruc = "None", betNames = "B1,B2", seed = 2)
    for (i in otherFiles) {
        newData <- createExternalParameters(20, names = "E0,ED50", 
            dataId = "ID", file = i, workingPath = parameters.datapath, 
            errStruc = "None", betNames = "B1,B2", seed = 2)
        expect_true(all(dim(dataNone) == dim(newData)), info = paste("Check correct dimensions when importing", 
            i))
        expect_true(all(names(dataNone) == names(newData)), info = paste("Check correct column names when importing", 
            i))
        expect_true(all(round(dataNone$ED50, 3) == newData$ED50), 
            info = paste("Check correct ED50 column when importing", 
                i))
        expect_true(all(round(dataNone$B1, 3) == newData$B1), 
            info = paste("Check correct B1 column when importing", 
                i))
    }
    dataAdd <- createExternalParameters(20, names = "E0,ED50", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        errStruc = "Add", betNames = "B1,B2", seed = 2)
    dataLogNorm <- createExternalParameters(20, names = "E0,ED50", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        errStruc = "Log", betNames = "B1,B2", seed = 2)
    dataProp <- createExternalParameters(20, names = "E0,ED50", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        errStruc = "Pro", betNames = "B1,B2", seed = 2)
    expect_true(identical(dataNone$E0 + dataNone$B1, dataAdd$E0), 
        info = "Testing the Additive errStruc")
    expect_true(identical(dataNone$ED50 + dataNone$B2, dataAdd$ED50), 
        info = "Testing the Additive errStruc")
    expect_true(identical(dataNone$E0 * exp(dataNone$B1), dataLogNorm$E0), 
        info = "Testing the Log-Normal errStruc")
    expect_true(identical(dataNone$ED50 * exp(dataNone$B2), dataLogNorm$ED50), 
        info = "Testing the Log-Normal errStruc")
    expect_true(identical(dataNone$E0 * (1 + dataNone$B1), dataProp$E0), 
        info = "Testing the Proportional errStruc")
    expect_true(identical(dataNone$ED50 * (1 + dataNone$B2), 
        dataProp$ED50), info = "Testing the Proportional errStruc")
    dataNone <- createExternalParameters(20, names = "E0,ED50", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        errStruc = "N", betNames = "B1,B2", seed = 2)
    dataAdd <- createExternalParameters(20, names = "E0,ED50", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        errStruc = "A", betNames = "B1,B2", betNums = "2,1", 
        seed = 2)
    expect_true(identical(dataNone$E0 + dataNone$B2, dataAdd$E0), 
        info = "Testing the Additive errStruc and betNums")
    expect_true(identical(dataNone$ED50 + dataNone$B1, dataAdd$ED50), 
        info = "Testing the Additive errStruc and betNums")
    dataNone <- createExternalParameters(20, names = "E0,ED50,EMAX", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        errStruc = "N", betNames = "B1,B2", betNums = "1,3", 
        seed = 2)
    dataAdd <- createExternalParameters(20, names = "E0,ED50,EMAX", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        errStruc = "A", betNames = "B1,B2", betNums = "1,3", 
        seed = 2)
    dataLogNorm <- createExternalParameters(20, names = "E0,ED50,EMAX", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        errStruc = "L", betNames = "B1,B2", betNums = "1,3", 
        seed = 2)
    dataProp <- createExternalParameters(20, names = "E0,ED50,EMAX", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        errStruc = "P", betNames = "B1,B2", betNums = "1,3", 
        seed = 2)
    expect_true(identical(dataNone$E0 + dataNone$B1, dataAdd$E0), 
        info = "Testing the Additive errStruc and betNums")
    expect_true(identical(dataNone$ED50, dataAdd$ED50), info = "Testing the Additive errStruc and betNums")
    expect_true(identical(dataNone$EMAX + dataNone$B2, dataAdd$EMAX), 
        info = "Testing the Additive errStruc and betNums")
    expect_true(identical(dataNone$E0 * exp(dataNone$B1), dataLogNorm$E0), 
        info = "Testing the Log-Normal errStruc and betNums")
    expect_true(identical(dataNone$ED50, dataLogNorm$ED50), info = "Testing the Log-Normal errStruc and betNums, variable without between should not be exponentiated when errStruc is prop")
    expect_true(identical(dataNone$EMAX * exp(dataNone$B2), dataLogNorm$EMAX), 
        info = "Testing the Log-Normal errStruc and betNums")
    expect_true(identical(dataNone$E0 * (1 + dataNone$B1), dataProp$E0), 
        info = "Testing the Proportional errStruc and betNums")
    expect_true(identical(dataNone$ED50, dataProp$ED50), info = "Testing the Proportional errStruc and betNums, variable without between should not be exponentiated when errStruc is prop")
    expect_true(identical(dataNone$EMAX * (1 + dataNone$B2), 
        dataProp$EMAX), info = "Testing the Proportional errStruc and betNums")
    expect_error(createExternalParameters(20, names = "E0,ED50,EMAX", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        refColName = "f09-124"), info = "wrong refColName")
    expect_error(createExternalParameters(20, names = "E0,ED50,EMAX", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        refColName = "REF"), info = "refColName not in the data")
    expect_error(createExternalParameters(20, names = "E0,ED50,EMAX", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        refColName = "ID", refCol = "1,2,3"), info = "refColName not in the data")
    testCovFile <- "testCovariates.csv"
    importCov <- createExternalCovariates(20, names = "X1", dataId = "ID", 
        file = testCovFile, workingPath = covariates.datapath, 
        refCol = "ID")
    refcolvalues <- importCov$ID.refCol
    importPar <- createExternalParameters(20, names = "B1,B2", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        refColName = "ID", refCol = refcolvalues)
    iData <- read.csv(file.path(parameters.datapath, testParamFile))[, 
        c("ID", "B1", "B2")]
    for (ref in refcolvalues) {
        expect_true(all(iData[iData$ID == ref, "B1"][1] == importPar[refcolvalues == 
            ref, "B1"]), info = "Testing the refCol system")
    }
    expect_error(createExternalParameters(20, names = "E0,ED50", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        betNames = "B1,B2", betNums = "1"), info = "#betNames != #betNums")
    expect_error(createExternalParameters(20, names = "E0,ED50", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        betNames = "B1,B2", betNums = "a,b"), info = "wrong betNums format")
    expect_error(createExternalParameters(20, names = "E0,ED50,EMAX", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        betNames = "B1,B2"), info = "Need betNums if index mismatch")
    expect_error(createExternalParameters(20, names = "E0,ED50,EMAX", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        subset = "@fewf-+fw"), info = "Non sense code")
    expect_error(createExternalParameters(20, names = "E0,ED50,EMAX", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        subset = "E0 < 0 < EMAX < 1 "), info = "Too many comparators")
    expect_error(createExternalParameters(20, names = "E0,ED50,EMAX", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        subset = "E0"), info = "Too few comparators")
    expect_true(all(createExternalParameters(20, names = "E0,ED50,EMAX", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        subset = "E0 < 0")$E0 < 0), info = "test that the subset is applied correctly")
    subData <- createExternalParameters(20, names = "E0,ED50,EMAX", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        subset = c("E0 < 0", "1 <= B1 <= 2"))
    expect_true(all(subData$E0 < 0), info = "test that the subset is applied correctly with multiple subsets")
    expect_true(all(subData$B1 >= 1 & subData$B1 <= 2), info = "test that the subset is applied correctly with two comparators")
    data1 <- createExternalParameters(20, names = "E0,ED50,EMAX", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        errStruc = "A", betNames = "B1,B2", betNums = "1,3", 
        seed = 81)
    rnorm(2100) + runif(2)
    data2 <- createExternalParameters(20, names = "E0,ED50,EMAX", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        errStruc = "A", betNames = "B1,B2", betNums = "1,3", 
        seed = 81)
    expect_equal(data2, data1)
})

test_that("test.data.param.norm", {
    expect_error(createNormalParameters(-50), info = "subjects should be positive")
    expect_error(createNormalParameters(50, names = "X, Y", mean = "0,a"), 
        info = "wrong mean")
    expect_error(createNormalParameters(50, names = "X,X", mean = "0,a"), 
        info = "duplicated names")
    expect_error(createNormalParameters(50, names = "X,Y", mean = "0,0,0"), 
        info = "dimension problem")
    expect_error(createNormalParameters(50, names = "X,Y,Z", 
        mean = "0,0,0", covariance = "1,0,0,-1,0,-1"), info = "wrong covariance matrix")
    expect_error(createNormalParameters(50, names = "X,Y,Z", 
        mean = "0,0,0", covariance = "1,0,0,1,0,1,2,3,4"), info = "wrong covariance matrix dims")
    expect_error(createNormalParameters(50, names = "X,Y,Z", 
        mean = "0,0,0", covariance = "1", maxDraws = -1), info = "wrong maxDraws value")
    expect_error(createNormalParameters(50, names = "X,Y,Z", 
        mean = "0,0,0", covariance = "1", maxDraws = 5:6), info = "wrong maxDraws value")
    expect_error(createNormalParameters(50, names = "X,Y,Z", 
        mean = "0,0,0", covariance = "1", digits = -1), info = "wrong digits value")
    expect_error(createNormalParameters(50, names = "X,Y,Z", 
        mean = "0,0,0", covariance = "1", digits = 1:2), info = "wrong digits value")
    expect_error(createNormalParameters(50, names = "X,Y,Z", 
        mean = "0,0,0", covariance = "1", betMean = "0", betNames = ".45", 
        betCov = "1"), info = "wrong betNames value")
    expect_error(createNormalParameters(50, names = "X,Y,Z", 
        mean = "0,0,0", covariance = "1", betMean = "0", betNames = "R", 
        betCov = "1"), info = "wrong betNames value, not a subset of names")
    expect_error(createNormalParameters(50, names = "X,Y,Z", 
        mean = "0,0,0", covariance = "1", betMean = "0", betNames = "X", 
        betCov = "-1"), info = "wrong betCov matrix")
    expect_error(createNormalParameters(50, names = "X,Y,Z", 
        mean = "0,0,0", covariance = "1", betMean = "0", betNames = "X", 
        betCov = "1,2,3"), info = "wrong betCov matrix")
    expect_error(createNormalParameters(50, names = "X,Y,Z", 
        mean = "0,0,0", covariance = "1", betMean = "0", betNames = "X", 
        betCov = "1", range = "]t43t534w-02354"), info = "non sense code")
    expect_error(createNormalParameters(50, names = "X,Y,Z", 
        mean = "0,0,0", covariance = "1", betMean = "0", betNames = "X", 
        betCov = "1", range = "X"), info = "wrong range code, not enough comparators")
    dataNone <- createNormalParameters(50, names = "X,Y,Z", mean = "0,0,0", 
        covariance = "1", betNames = "X,Y,Z", betCov = "1", betMean = "0,0,0", 
        errStruc = "N", seed = 99)
    dataAdd <- createNormalParameters(50, names = "X,Y,Z", mean = "0,0,0", 
        covariance = "1", betNames = "X,Y,Z", betCov = "1", betMean = "0,0,0", 
        errStruc = "A", seed = 99)
    dataLogNorm <- createNormalParameters(50, names = "X,Y,Z", 
        mean = "0,0,0", covariance = "1", betNames = "X,Y,Z", 
        betCov = "1", betMean = "0,0,0", errStruc = "L", seed = 99)
    errStrucDiff <- round(dataAdd[, c("X", "Y", "Z")] - (dataNone[, 
        c("X", "Y", "Z")] + dataNone[, paste(c("X", "Y", "Z"), 
        ".Between", sep = "")]), 2)
    expect_true(all(unlist(errStrucDiff == 0)), info = "checking the errStruc")
    expect_true(all(dataLogNorm[, c("X", "Y", "Z")] - dataNone[, 
        c("X", "Y", "Z")] * exp(dataNone[, paste(c("X", "Y", 
        "Z"), ".Between", sep = "")] == 0)), info = "checking the errStruc with None and Log Normal")
    dataNone <- createNormalParameters(50, names = "X,Y,Z", mean = "0,50,100", 
        covariance = "1", betNames = "X,Y", betCov = "1", betMean = "0,0", 
        errStruc = "None", seed = 99, digits = 10)
    dataNone$Z.Between <- rep(0, nrow(dataNone))
    dataAdd <- createNormalParameters(50, names = "X,Y,Z", mean = "0,50,100", 
        covariance = "1", betNames = "X,Y", betCov = "1", betMean = "0,0", 
        errStruc = "Add", seed = 99, digits = 10)
    dataLogNorm <- createNormalParameters(50, names = "X,Y,Z", 
        mean = "0,50,100", covariance = "1", betNames = "X,Y", 
        betCov = "1", betMean = "0,0", errStruc = "Log", seed = 99, 
        digits = 10)
    .roundIt <- MSToolkit:::.roundIt
    expect_true(identical(.roundIt(dataAdd[, c("X", "Y")], 5), 
        .roundIt(dataNone[, c("X", "Y")] + dataNone[, paste(c("X", 
            "Y"), ".Between", sep = "")], 5)), info = "check the errStruc, not all between, None and Add")
    v1 <- dataLogNorm[, c("Z", "Y")]
    v2 <- dataNone[, c("Z", "Y")] * exp(dataNone[, paste(c("Z", 
        "Y"), ".Between", sep = "")])
    expect_equal(v2, v1, info = "\ncheck the errStruc, not all between, None and Prop, different order(1)")
    expect_true(identical(dataAdd$Z, dataNone$Z), info = "check the errStruc, not between, None and Add")
    expect_true(identical(dataLogNorm$Z, dataNone$Z), info = "check the errStruc, not between, None and Prop")
    dataNone <- createNormalParameters(50, names = "X,Y,Z", mean = "0,50,100", 
        covariance = "1", betNames = "Z,Y", betCov = "1", betMean = "0,0", 
        errStruc = "N", seed = 99, digits = 5)
    dataAdd <- createNormalParameters(50, names = "X,Y,Z", mean = "0,50,100", 
        covariance = "1", betNames = "Z,Y", betCov = "1", betMean = "0,0", 
        errStruc = "A", seed = 99, digits = 5)
    dataLogNorm <- createNormalParameters(50, names = "X,Y,Z", 
        mean = "0,50,100", covariance = "1", betNames = "Z,Y", 
        betCov = "1", betMean = "0,0", errStruc = "L", seed = 99, 
        digits = 5)
    expect_true(all(.roundIt(dataAdd[, c("Z", "Y")] - (dataNone[, 
        c("Z", "Y")] + dataNone[, paste(c("Z", "Y"), ".Between", 
        sep = "")]), 3) == 0), info = "check the errStruc, not all between, None and Prop, different order")
    v1 <- dataLogNorm[, c("Z", "Y")]
    v2 <- dataNone[, c("Z", "Y")] * exp(dataNone[, paste(c("Z", 
        "Y"), ".Between", sep = "")])
    expect_equal(v2, v1, info = "\ncheck the errStruc, not all between, None and Prop, different order(2)")
    expect_true(all(dataAdd$X - dataNone$X == 0), info = "check the errStruc, not between, None and Prop, different order")
    expect_true(all(dataLogNorm$X - dataNone$X == 0), info = "check the errStruc, not between, None and Prop, different order")
    dataFixed <- createNormalParameters(50, names = "X,Y,Z", 
        mean = "0,50,100", covariance = "1")
    for (va in c("X", "Y", "Z")) {
        expect_true(all(diff(dataFixed[[va]]) == 0), info = "check the fixed parameters")
    }
})

test_that("test.data.param.wrap", {
    testParamFile <- "testParam.csv"
    wrongTestParamFile <- "wrongTestParam.csv"
    expect_error(createParameters(-10), info = "wrong subject (negative)")
    expect_error(createParameters(10, idCol = "ID, SUB"), info = "wrong id (too long)")
    expect_error(createParameters(10, idCol = "08234ID"), info = "wrong id")
    expect_error(createParameters(10, flagName = "PAROMIT, OMIT"), 
        info = "wrong flagName (too long)")
    expect_error(createParameters(10, idCol = "082PAROMIT"), 
        info = "wrong flagName")
    d1 <- createNormalParameters(50, names = "X,Y,Z", mean = "0,0,0", 
        covariance = "1", betNames = "X,Y,Z", betCov = "1", betMean = "0,0,0", 
        errStruc = "A", seed = 99)
    d2 <- createParameters(50, genNames = "X,Y,Z", genFixedMean = "0,0,0", 
        genFixedCov = "1", genBetweenNames = "X,Y,Z", genBetweenCov = "1", 
        genBetweenMean = "0,0,0", genErrStruc = "A", seed = 99)
    expect_true(identical(d1, d2), info = "only normal")
    d1 <- createExternalParameters(20, names = "E0,ED50,EMAX", 
        file = testParamFile, workingPath = parameters.datapath, 
        dataId = "ID", errStruc = "A", betNames = "B1,B2", betNums = "1,3", 
        seed = 81)
    d2 <- createParameters(20, extNames = "E0,ED50,EMAX", extDataId = "ID", 
        extFile = testParamFile, workingPath = parameters.datapath, 
        extErrStruc = "A", extBetween = "B1,B2", extBetweenNums = "1,3", 
        seed = 81)
    expect_true(identical(d1, d2), info = "only external")
    dNor <- createNormalParameters(50, names = "X,Y,Z", mean = "0,0,0", 
        covariance = "1", betNames = "X,Y,Z", betCov = "1", betMean = "0,0,0", 
        errStruc = "A", seed = 81)
    dExt <- createExternalParameters(50, names = "E0,ED50,EMAX", 
        dataId = "ID", file = testParamFile, workingPath = parameters.datapath, 
        errStruc = "A", betNames = "B1,B2", betNums = "1,3", 
        seed = 81)
    dAll <- createParameters(50, extNames = "E0,ED50,EMAX", extFile = testParamFile, 
        workingPath = parameters.datapath, extErrStruc = "A", 
        extBetween = "B1,B2", extBetweenNums = "1,3", extDataId = "ID", 
        seed = 81, genNames = "X,Y,Z", genFixedMean = "0,0,0", 
        genFixedCov = "1", genBetweenNames = "X,Y,Z", genBetweenCov = "1", 
        genBetweenMean = "0,0,0", genErrStruc = "A")
    expect_true(identical(dNor[, c("SUBJ", "X", "Y", "Z")], dAll[, 
        c("SUBJ", "X", "Y", "Z")]), info = "all + nor")
    expect_true(identical(dExt[, c("SUBJ", "E0", "ED50", "EMAX")], 
        dAll[, c("SUBJ", "E0", "ED50", "EMAX")]), info = "all + ext")
    dNor <- createNormalParameters(50, names = "X,Y,Z", mean = "0,0,0", 
        covariance = "1", betNames = "X,Y,Z", betCov = "1", betMean = "0,0,0", 
        errStruc = "A", seed = 81, range = "Y < 3")
    dExt <- createExternalParameters(50, names = "E0,ED50,EMAX", 
        file = testParamFile, workingPath = parameters.datapath, 
        dataId = "ID", errStruc = "A", betNames = "B1,B2", betNums = "1,3", 
        seed = 81, )
    dAll <- createParameters(50, extNames = "E0,ED50,EMAX", extFile = testParamFile, 
        workingPath = parameters.datapath, extErrStruc = "A", 
        extBetween = "B1,B2", extBetweenNums = "1,3", extDataId = "ID", 
        seed = 81, genNames = "X,Y,Z", genFixedMean = "0,0,0", 
        genFixedCov = "1", genBetweenNames = "X,Y,Z", genBetweenCov = "1", 
        genBetweenMean = "0,0,0", genErrStruc = "A", genRange = "Y < 3")
    expect_true(identical(dNor[, c("SUBJ", "X", "Y", "Z")], dAll[, 
        c("SUBJ", "X", "Y", "Z")]), info = "all + nor")
    expect_true(identical(dExt[, c("SUBJ", "E0", "ED50", "EMAX")], 
        dAll[, c("SUBJ", "E0", "ED50", "EMAX")]), info = "all + ext")
    expect_equal(dAll$PAROMIT, dNor$PAROMIT, info = "checking the PAROMIT")
})

test_that("test.parameter.rangeExclude", {
    expect_error(createNormalParameters(500, names = "X,Y,Z", 
        mean = "0,50,100", covariance = "1", betNames = "X,Y", 
        betCov = "1", errStruc = "A", seed = 99, range = "Y < 50", 
        maxDraws = 1, parRangeTolerance = 0.9), info = "Bad range specified")
    expect_true(is.data.frame(createNormalParameters(500, names = "X,Y,Z", 
        mean = "0,50,100", covariance = "1", betNames = "X,Y", 
        betCov = "1", errStruc = "A", seed = 99, range = "Y < 100", 
        maxDraws = 1, parRangeTolerance = 0.9)), info = "Good range specified")
    warnData <- suppressWarnings(createNormalParameters(500, 
        names = "X,Y,Z", mean = "0,50,100", covariance = "1", 
        betNames = "X,Y", betCov = "1", errStruc = "A", seed = 99, 
        range = "Y < 50", parRangeTolerance = 0.1, maxDraws = 1))
    expect_true(is.data.frame(warnData) & any(warnData$PAROMIT == 
        1), info = "Dataset returned with parameter omits")
})

test_that("test.parameter.sf3", {
    expect_error(createNormalParameters(50, names = "X,Y,Z", 
        mean = "100,100,100", covariance = "1", betNames = "X,Y", 
        betCov = "1", betMean = "0,0", errStruc = "N", digits = -1), 
        info = "digits should be positive")
    dataNone <- createNormalParameters(50, names = "X,Y,Z", mean = "100,100,100", 
        covariance = "1", betNames = "X,Y", betCov = "1", betMean = "0,0", 
        errStruc = "N", digits = "2")
    dataAdd <- createNormalParameters(50, names = "X,Y,Z", mean = "100,100,100", 
        covariance = "1", betNames = "X,Y", betCov = "1", betMean = "0,0", 
        errStruc = "A", digits = "2")
    dataLogNorm <- createNormalParameters(50, names = "X,Y,Z", 
        mean = "100,100,100", covariance = "1", betNames = "X,Y", 
        betCov = "1", betMean = "0,0", errStruc = "L", digits = "2")
    expect_equal(round(dataAdd[, c("X", "Y", "Z")], 2), dataAdd[, 
        c("X", "Y", "Z")], info = "check the atomic digits (add)")
    expect_equal(round(dataLogNorm[, c("X", "Y", "Z")], 2), dataLogNorm[, 
        c("X", "Y", "Z")], info = "check the atomic digits (prop)")
    expect_equal(round(dataNone[, c("X", "Y", "Z", "X.Between", 
        "Y.Between")], 2), dataNone[, c("X", "Y", "Z", "X.Between", 
        "Y.Between")], info = "check the atomic digits (prop)")
    dataNone <- createNormalParameters(50, names = "X,Y,Z", mean = "100,100,100", 
        covariance = "1", betNames = "X,Y", betCov = "1", betMean = "0,0", 
        errStruc = "N", digits = "2,3,2")
    dataAdd <- createNormalParameters(50, names = "X,Y,Z", mean = "100,100,100", 
        covariance = "1", betNames = "X,Y", betCov = "1", betMean = "0,0", 
        errStruc = "A", digits = "2,3,2")
    dataLogNorm <- createNormalParameters(50, names = "X,Y,Z", 
        mean = "100,100,100", covariance = "1", betNames = "X,Y", 
        betCov = "1", betMean = "0,0", errStruc = "L", digits = "2,3,2")
    expect_equal(round(dataAdd$X, 2), dataAdd$X, info = "check the vector digits (add,X)")
    expect_equal(round(dataAdd$Y, 3), dataAdd$Y, info = "check the vector digits (add,Y)")
    expect_equal(round(dataAdd$Z, 2), dataAdd$Z, info = "check the vector digits (add,Y)")
    expect_equal(round(dataLogNorm$X, 2), dataLogNorm$X, info = "check the vector digits (Prop,X)")
    expect_equal(round(dataLogNorm$Y, 3), dataLogNorm$Y, info = "check the vector digits (Prop,Y)")
    expect_equal(round(dataLogNorm$Z, 2), dataLogNorm$Z, info = "check the vector digits (Prop,Y)")
    expect_equal(round(dataNone$X, 2), dataNone$X, info = "check the vector digits (None,X)")
    expect_equal(round(dataNone$Y, 3), dataNone$Y, info = "check the vector digits (None,Y)")
    expect_equal(round(dataNone$Z, 2), dataNone$Z, info = "check the vector digits (None,Y)")
    expect_equal(round(dataNone$X.Between, 2), dataNone$X.Between, 
        info = "check the vector digits (None,X.Between)")
    expect_equal(round(dataNone$Y.Between, 3), dataNone$Y.Between, 
        info = "check the vector digits (None,Y.Between)")
})

test_that("test.parameter.sf8", {
    dataNone <- createNormalParameters(50, names = "X,Y,Z", mean = "0,50,100", 
        betNames = "X,Y", covariance = "1", betMean = "0,0", 
        errStruc = "N", seed = 99)
    expect_true(all(dataNone[, c("X.Between", "Y.Between")] == 
        0), info = "checking 0 covariance by default for between effects")
    data.sf8 <- createNormalParameters(500, names = "X,Y,Z", 
        mean = "0,50,100", seed = 99)
    expect_true(all(sweep(data.sf8[, c("X", "Y", "Z")], 2, c(0, 
        50, 100), "-") == 0), info = "checking the 0 covariance by default")
})

