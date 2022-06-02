setEctdDataMethod("CSV")
resetEctdColNames()

systemTestPath <- MSToolkit:::testthat_path
interimPath <- file.path(systemTestPath, "systemTest", "data", "Interim")
scriptsPath <- file.path(systemTestPath, "systemTest", "data", "Scripts")

test_that("test.analyseData.BasicAnalysis", {

  # systemTestPath <- system.file(package = "MSToolkit", "tests", "testthat", "systemTest", "data")
  # interimPath <- file.path(systemTestPath, "Interim")
  # scriptsPath <- file.path(systemTestPath, "Scripts")

  # Allow analysis of all, or a subset of, generated simulated data from a single function call
  setEctdDataMethod("CSV")
  whichPath <- interimPath
  lmCode <- function(data) {
    myLm <- lm(RESP ~ DOSE, data = data)
    newData <- data.frame(DOSE = sort(unique(data$DOSE)))
    doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
    names(doseMeans) <- c("Mean", "Lower", "Upper")
    data.frame(DOSE = sort(unique(data$DOSE)), doseMeans, N = as.vector(table(data$DOSE)))
  }

  iCode <- function(data, uniDoses = c(0, 5, 25, 50, 100)) {
    zeroUpper <- data$Upper[1]
    whichLower <- data$Lower < zeroUpper
    whichLower[1] <- FALSE
    if (any(whichLower)) list(DROP = uniDoses [whichLower])
    else list()
  }

  mCode <- function(data, doseCol, interimCol) {
    data.frame(DROPPED = sum(data$DROPPED), TEST = 1)
  }

  # Delete Files
  targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }

  analyzeData(replicates = 1:3, analysisCode = lmCode, macroCode = mCode, interimCode = iCode, workingPath = whichPath)

  expect_true(all(file.exists(file.path(whichPath, targetFiles))))
  microFiles <- list.files(file.path(whichPath, "MicroEvaluation"))
  macroFiles <- list.files(file.path(whichPath, "MacroEvaluation"))
  expect_true(all(microFiles == paste("micro000", 1:3, ".csv", sep="")))
  expect_true( all(macroFiles == paste("macro000", 1:3, ".csv", sep="")))

  getMicro <- read.csv(file.path(whichPath, "microSummary.csv"))
  getMacro <- read.csv(file.path(whichPath, "macroSummary.csv"))
  expect_true(nrow(getMacro) == 3 & all(getMacro$Replicate == 1:3) & all(getMacro$DROPPED == 1) & all(getMacro$TEST == 1))

  splitMicro <- lapply(split(getMicro[-1], getMicro$Replicate), function(df) {
    row.names(df) <- 1:nrow(df)
    df
  })
  expect_true(length(splitMicro) == 3)
  expect_true(identical(splitMicro[[1]], splitMicro[[3]]))

  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }
})


test_that("test.analyseData.RScript", {

  # systemTestPath <- system.file(package = "MSToolkit", "tests", "testthat", "systemTest", "data")
  # interimPath <- file.path(systemTestPath, "Interim")
  # scriptsPath <- file.path(systemTestPath, "Scripts")

  # Allow analysis based on an R function, external R script or
  # external SAS script

  # "Based on an R Function" is tested in previous
  # test, so just need to focus on scripts here

  # Setup
  whichPath <- scriptsPath
  setEctdDataMethod("CSV")

  iCode <- function(data, uniDoses = c(0, 5, 25, 50, 100)) {
    zeroUpper <- data$Upper[1]
    whichLower <- data$Lower < zeroUpper
    whichLower[1] <- FALSE
    list(KEEP = !whichLower)
  }

  mCode <- function(data, doseCol, interimCol) {
    data.frame(DROPPED = sum(data$DROPPED), TEST = 1)
  }

  targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }

  #
  # R Script Calling ...
  #

  # Delete Files
  analyzeData(analysisCode = "rAnalysisScript.R",
              macroCode = mCode, interimCode = iCode, workingPath = whichPath)

  getMicro <- read.csv(file.path(whichPath, "microSummary.csv"))
  getMacro <- read.csv(file.path(whichPath, "macroSummary.csv"))
  expect_true(nrow(getMacro) == 3 & nrow(getMicro) == 60)
  expect_true(!any(is.na(getMicro$Mean)))

  # Clean up files before next go
  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }
})


test_that("test.analyseData.SASScript", {

  # systemTestPath <- system.file(package = "MSToolkit", "tests", "testthat", "systemTest", "data")
  # interimPath <- file.path(systemTestPath, "Interim")
  # scriptsPath <- file.path(systemTestPath, "Scripts")

  # Check for SAS existance
  isWin <- .Platform$OS.type == "windows"
  getPath <- if (isWin) getEctdExternalPath("SASPATH_WIN") else getEctdExternalPath("SASPATH_UNIX")
  if (nchar(getPath) > 1 && file.exists(getPath)) {

    # Setup
    whichPath <- scriptsPath
    setEctdDataMethod("CSV")

    iCode <- function(data, uniDoses = c(0, 5, 25, 50, 100)) {
      zeroUpper <- data$Upper[1]
      whichLower <- data$Lower < zeroUpper
      whichLower[1] <- FALSE
      list(KEEP = !whichLower)
    }

    mCode <- function(data, doseCol, interimCol) {
      data.frame(DROPPED = sum(data$DROPPED), TEST = 1)
    }

    targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
    if (any(file.exists(file.path(whichPath, targetFiles)))) {
      for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
    }

    analyzeData(analysisCode = "analysisCode.sas", software = "SAS",
                macroCode = mCode, interimCode = iCode, workingPath = whichPath)

    getMicro <- read.csv(file.path(whichPath, "microSummary.csv"))
    getMacro <- read.csv(file.path(whichPath, "macroSummary.csv"))
    expect_true(nrow(getMacro) == 3 & nrow(getMicro) == 60)
    expect_true(!any(is.na(getMicro$Mean)))

    # Clean up files before next go
    if (any(file.exists(file.path(whichPath, targetFiles)))) {
      for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
    }
  } else {
      expect_false(FALSE)
      message("Error: (CAN NOT TEST > 'SAS' NOT FOUND)")}
})


test_that("test.analyseData.Interims", {

  # systemTestPath <- system.file(package = "MSToolkit", "tests", "testthat", "systemTest", "data")
  # interimPath <- file.path(systemTestPath, "Interim")
  # scriptsPath <- file.path(systemTestPath, "Scripts")

  # Allow automated interim analysis step and data update based on of microevluation outputs
  whichPath <- interimPath
  setEctdDataMethod("CSV")

  lmCode <- function(data) {
    myLm <- lm(RESP ~ DOSE, data = data)
    newData <- data.frame(DOSE = sort(unique(data$DOSE)))
    doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
    names(doseMeans) <- c("Mean", "Lower", "Upper")
    data.frame(DOSE = sort(unique(data$DOSE)), doseMeans, N = as.vector(table(data$DOSE)))
  }

  iCode <- function(data, uniDoses = c(0, 5, 25, 50, 100)) {
    zeroUpper <- data$Upper[1]
    whichLower <- data$Lower < zeroUpper
    whichLower[1] <- FALSE
    list(KEEP = !whichLower)
  }

  mCode <- function(data, doseCol, interimCol) {
    data.frame(DROPPED = sum(data$DROPPED), TEST = 1)
  }

  # Delete Files
  targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }

  analyzeData(replicates = 1:3, analysisCode = lmCode, macroCode = mCode,
              interimCode = iCode, workingPath = whichPath)

  expect_true(all(file.exists(file.path(whichPath, targetFiles))))
  microFiles <- list.files(file.path(whichPath, "MicroEvaluation"))
  macroFiles <- list.files(file.path(whichPath, "MacroEvaluation"))
  expect_true(all(microFiles == paste("micro000", 1:3, ".csv", sep="")))
  expect_true(all(macroFiles == paste("macro000", 1:3, ".csv", sep="")))

  getMicro <- read.csv(file.path(whichPath, "microSummary.csv"))
  getMacro <- read.csv(file.path(whichPath, "macroSummary.csv"))
  expect_true(nrow(getMacro) == 3 & all(getMacro$Replicate == 1:3) & all(getMacro$DROPPED == 1) & all(getMacro$TEST == 1))
  splitMicro <- lapply(split(getMicro[-1], getMicro$Replicate), function(df) {
    row.names(df) <- 1:nrow(df)
    df
  })
  expect_true(length(splitMicro) == 3)
  expect_true(identical(splitMicro[[1]], splitMicro[[3]]))

  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }
})


test_that("test.analyseData.MacroEval", {

  # systemTestPath <- system.file(package = "MSToolkit", "tests", "testthat", "systemTest", "data")
  # interimPath <- file.path(systemTestPath, "Interim")
  # scriptsPath <- file.path(systemTestPath, "Scripts")

  # Allow automated macro evaluation of microevaluation outputs
  whichPath <- interimPath
  setEctdDataMethod("CSV")

  lmCode <- function(data) {
    myLm <- lm(RESP ~ DOSE, data = data)
    newData <- data.frame(DOSE = sort(unique(data$DOSE)))
    doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
    names(doseMeans) <- c("Mean", "Lower", "Upper")
    data.frame(DOSE = sort(unique(data$DOSE)), doseMeans, N = as.vector(table(data$DOSE)))
  }

  iCode <- function(data, uniDoses = c(0, 5, 25, 50, 100)) {
    zeroUpper <- data$Upper[1]
    whichLower <- data$Lower < zeroUpper
    whichLower[1] <- FALSE
    list(KEEP = !whichLower)
  }

  mCode <- function(data, doseCol, interimCol) {
    data.frame(DROPPED = sum(data$DROPPED), TEST = 1)
  }

  # Delete Files
  targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }

  analyzeData(replicates = 1:3, analysisCode = lmCode, macroCode = mCode,
              interimCode = iCode, workingPath = whichPath)

  expect_true(all(file.exists(file.path(whichPath, targetFiles))))
  microFiles <- list.files(file.path(whichPath, "MicroEvaluation"))
  macroFiles <- list.files(file.path(whichPath, "MacroEvaluation"))
  expect_true(all(microFiles == paste("micro000", 1:3, ".csv", sep="")))
  expect_true(all(macroFiles == paste("macro000", 1:3, ".csv", sep="")))

  getMicro <- read.csv(file.path(whichPath, "microSummary.csv"))
  getMacro <- read.csv(file.path(whichPath, "macroSummary.csv"))
  expect_true(nrow(getMacro) == 3 & all(getMacro$Replicate == 1:3) & all(getMacro$DROPPED == 1) & all(getMacro$TEST == 1))
  splitMicro <- lapply(split(getMicro[-1], getMicro$Replicate), function(df) {
    row.names(df) <- 1:nrow(df)
    df
  })
  expect_true(length(splitMicro) == 3)
  expect_true(identical(splitMicro[[1]], splitMicro[[3]]))

  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }
})


test_that("test.analyseData.gridCalling", {

  # systemTestPath <- system.file(package = "MSToolkit", "tests", "testthat", "systemTest", "data")
  # interimPath <- file.path(systemTestPath, "Interim")
  # scriptsPath <- file.path(systemTestPath, "Scripts")

  if (MSToolkit:::.checkGridAvailable()) {
    whichPath <- interimPath
    lmCode <- function(data) {
      myLm <- lm(RESP ~ DOSE, data = data)
      newData <- data.frame(DOSE = sort(unique(data$DOSE)))
      doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
      names(doseMeans) <- c("Mean", "Lower", "Upper")
      data.frame(DOSE = sort(unique(data$DOSE)), doseMeans, N = as.vector(table(data$DOSE)))
    }

    iCode <- function(data, uniDoses = c(0, 5, 25, 50, 100)) {
      zeroUpper <- data$Upper[1]
      whichLower <- data$Lower < zeroUpper
      whichLower[1] <- FALSE
      list(KEEP = !whichLower)
      Sys.sleep(1)
    }

    mCode <- function(data, doseCol, interimCol) {
      data.frame(DROPPED = sum(data$DROPPED), TEST = 1)
    }

    # Delete Files
    targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
    if (any(file.exists(file.path(whichPath, targetFiles)))) {
      for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
    }

    analyzeData(replicates = 1:6, analysisCode = lmCode, macroCode = mCode,
                interimCode = iCode, workingPath = whichPath, grid = TRUE)
    microFiles <- file.info(dir(file.path(whichPath, "MicroEvaluation"), full.names = TRUE))
    orderTime <- order(microFiles$mtime)
    orderName <- order(rownames(microFiles))
    expect_true(!all(orderTime == orderName ))
  } else {
    expect_true(FALSE , " (CAN NOT TEST - 'GRID' NOT AVAILABLE)")
  }
})


test_that("test.analyseData.MicroMacroFiles", {

  # systemTestPath <- system.file(package = "MSToolkit", "tests", "testthat", "systemTest", "data")
  # interimPath <- file.path(systemTestPath, "Interim")
  # scriptsPath <- file.path(systemTestPath, "Scripts")

  # Allow compilation microevalation and macroevaluation, along with writing to external CSV files
  whichPath <- interimPath
  setEctdDataMethod("CSV")

  lmCode <- function(data) {
    myLm <- lm(RESP ~ DOSE, data = data)
    newData <- data.frame(DOSE = sort(unique(data$DOSE)))
    doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
    names(doseMeans) <- c("Mean", "Lower", "Upper")
    data.frame(DOSE = sort(unique(data$DOSE)), doseMeans, N = as.vector(table(data$DOSE)))
  }

  iCode <- function(data, uniDoses = c(0, 5, 25, 50, 100)) {
    zeroUpper <- data$Upper[1]
    whichLower <- data$Lower < zeroUpper
    whichLower[1] <- FALSE
    list(KEEP = !whichLower)
  }

  mCode <- function(data, doseCol, interimCol) {
    data.frame(DROPPED = sum(data$DROPPED), TEST = 1)
  }

  # Delete Files
  targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }

  analyzeData(replicates = 1:3, analysisCode = lmCode, macroCode = mCode,
              interimCode = iCode, workingPath = whichPath)

  expect_true(all(file.exists(file.path(whichPath, targetFiles))))
  microFiles <- list.files(file.path(whichPath, "MicroEvaluation"))
  macroFiles <- list.files(file.path(whichPath, "MacroEvaluation"))
  expect_true(all(microFiles == paste("micro000", 1:3, ".csv", sep="")))
  expect_true(all(macroFiles == paste("macro000", 1:3, ".csv", sep="")))

  getMicro <- read.csv(file.path(whichPath, "microSummary.csv"))
  getMacro <- read.csv(file.path(whichPath, "macroSummary.csv"))
  expect_true(nrow(getMacro) == 3 & all(getMacro$Replicate == 1:3) & all(getMacro$DROPPED == 1) & all(getMacro$TEST == 1))
  splitMicro <- lapply(split(getMicro[-1], getMicro$Replicate), function(df) {
    row.names(df) <- 1:nrow(df)
    df
  })
  expect_true(length(splitMicro) == 3)
  expect_true(identical(splitMicro[[1]], splitMicro[[3]]))

  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }
})


test_that("test.analyseData.OmitRows", {

  # systemTestPath <- system.file(package = "MSToolkit", "tests", "testthat", "systemTest", "data")
  # interimPath <- file.path(systemTestPath, "Interim")
  # scriptsPath <- file.path(systemTestPath, "Scripts")

  # Allow automated removal of rows where the missing,
  # parameter or response flags are set
  whichPath <- interimPath
  setEctdDataMethod("CSV")

  lmCode <- function(data) data.frame(DOSE = sort(unique(data$DOSE)), N = as.vector(table(data$DOSE)))

  mCode <- function(data, doseCol, interimCol) data.frame(N = sum(data$N))

  # Delete Files
  targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }

  analyzeData(replicates = 4:6, analysisCode = lmCode, macroCode = mCode, workingPath = whichPath,
              removeMissing = FALSE, removeParOmit = FALSE, removeRespOmit = FALSE)
  getMacro.000 <- read.csv(file.path(whichPath, "macroSummary.csv"))$N[1]

  analyzeData(replicates = 4:6, analysisCode = lmCode, macroCode = mCode, workingPath = whichPath,
              removeMissing = TRUE, removeParOmit = FALSE, removeRespOmit = FALSE)
  getMacro.100 <- read.csv(file.path(whichPath, "macroSummary.csv"))$N[1]

  analyzeData(replicates = 4:6, analysisCode = lmCode, macroCode = mCode, workingPath = whichPath,
              removeMissing = FALSE, removeParOmit = TRUE, removeRespOmit = FALSE)
  getMacro.010 <- read.csv(file.path(whichPath, "macroSummary.csv"))$N[1]

  analyzeData(replicates = 4:6, analysisCode = lmCode, macroCode = mCode, workingPath = whichPath,
              removeMissing = FALSE, removeParOmit = FALSE, removeRespOmit = TRUE)
  getMacro.001 <- read.csv(file.path(whichPath, "macroSummary.csv"))$N[1]

  analyzeData(replicates = 4:6, analysisCode = lmCode, macroCode = mCode, workingPath = whichPath)
  getMacro.111 <- read.csv(file.path(whichPath, "macroSummary.csv"))$N[1]

  targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }

  nVals <- c(getMacro.000, getMacro.100, getMacro.010, getMacro.001, getMacro.111)
  expect_true(all(nVals == c(400, 364, 333, 333, 242)))
})


test_that("test.analyseData.RandomSeed", {

  # systemTestPath <- system.file(package = "MSToolkit", "tests", "testthat", "systemTest", "data")
  # interimPath <- file.path(systemTestPath, "Interim")
  # scriptsPath <- file.path(systemTestPath, "Scripts")

  # Allow control over random number seed for reproducibility
  whichPath <- interimPath
  setEctdDataMethod("CSV")

  lmCode <- function(data) data.frame(DOSE = sort(unique(data$DOSE)), RAND = rnorm(nrow(data)))

  mCode <- function(data, doseCol, interimCol) data.frame(MEAN = round(mean(data$RAND), 2))

  # Delete Files
  targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }
  analyzeData(replicates = 1:2, analysisCode = lmCode, macroCode = mCode, workingPath = whichPath, seed = 111)
  getMacro1 <- read.csv(file.path(whichPath, "macroSummary.csv"))

  targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }

  analyzeData(replicates = 1:2, analysisCode = lmCode, macroCode = mCode, workingPath = whichPath, seed = 111)
  getMacro2 <- read.csv(file.path(whichPath, "macroSummary.csv"))

  targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }

  expect_true(identical(getMacro1, getMacro2))
})


test_that("test.analyseData.ColumnNames", {

  # systemTestPath <- system.file(package = "MSToolkit", "tests", "testthat", "systemTest", "data")
  # interimPath <- file.path(systemTestPath, "Interim")
  # scriptsPath <- file.path(systemTestPath, "Scripts")

  # Allow identification of key columns within the data, such as
  # the "Dose", "Missing" and "Interim" columns
  whichPath <- interimPath
  resetEctdColNames()

  lmCode1 <- function(data) {
    myLm <- lm(respcol ~ dosecol, data = data)
    newData <- data.frame(dosecol = sort(unique(data$dosecol)))
    doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
    names(doseMeans) <- c("Mean", "Lower", "Upper")
    data.frame(dosecol = sort(unique(data$dosecol)), doseMeans, N = as.vector(table(data$dosecol)))
  }

  lmCode2 <- function(data) {
    myLm <- lm(RESP ~ DOSE, data = data)
    newData <- data.frame(DOSE = sort(unique(data$DOSE)))
    doseMeans <- as.data.frame(predict(myLm, newData, se.fit = TRUE, interval = "confidence")$fit)
    names(doseMeans) <- c("Mean", "Lower", "Upper")
    data.frame(DOSE = sort(unique(data$DOSE)), doseMeans, N = as.vector(table(data$DOSE)))
  }

  iCode <- function(data, uniDoses = c(0, 5, 25, 50, 100)) {
    zeroUpper <- data$Upper[1]
    whichLower <- data$Lower < zeroUpper
    whichLower[1] <- FALSE
    list(KEEP = !whichLower)
  }

  mCode <- function(data, doseCol, interimCol) {
    data.frame(DROPPED = sum(data$DROPPED), TEST = 1)
  }

  # Delete Files
  targetFiles <- c("microSummary.csv", "macroSummary.csv", "MicroEvaluation", "MacroEvaluation")
  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }

  analyzeData(replicates = 7:8, analysisCode = lmCode1, macroCode = mCode, interimCode = iCode,
              workingPath = whichPath, doseCol = "dosecol", parOmitFlag = "paromitcol",
              missingFlag = "missingcol", interimCol = "interimcol", respOmitFlag = "respomitcol")
  getMicro1 <- read.csv(file.path(whichPath, "microSummary.csv"))
  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }

  analyzeData(replicates = 5:6, analysisCode = lmCode2, macroCode = mCode, interimCode = iCode,
              workingPath = whichPath)
  getMicro2 <- read.csv(file.path(whichPath, "microSummary.csv"))
  if (any(file.exists(file.path(whichPath, targetFiles)))) {
    for (i in targetFiles) try(unlink(file.path(whichPath, i), recursive = TRUE), silent = TRUE)
  }

  names(getMicro1) <- names(getMicro2)
  expect_true(identical(getMicro1, getMicro2))
})