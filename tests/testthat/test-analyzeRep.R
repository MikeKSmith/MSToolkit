setEctdDataMethod("CSV")

if( !exists("unitTestPath")) unitTestPath <- system.file(package = "MSToolkit", "Runit")
analyseRep.datapath <- file.path( unitTestPath , "data", "analyseRep" )
cat("analyseRep.datapath:", analyseRep.datapath, "\n" )

test_that("test_analyzeRep_rep", {
  # test the argument of replicate
  expect_error(analyzeRep(replicate = 2:7),
               msg = "check the handling of the replicate arg, must be of length one")
  expect_error(analyzeRep(replicate = -3),
               msg = "check the handling of the replicate arg, must be positive")
  expect_error(analyzeRep(replicate = "notanumber"),
               msg = "check the handling of the replicate arg, must be a number")

  wrongfun <- function(notdata){
    notdata
  }
  # test the argument of analysisCode
  expect_error(analyzeRep(replicate = 1,
                          analysisCode = wrongfun),
               msg = "analysis code must have a data argument")
  expect_error(analyzeRep( replicate = 1,
                           analysisCode = "doesnotexists"),
               msg = "analysis code must be available")

  dummyAnalysisCode <- function(data){data}

  # test the argument of doseCol
  expect_error(analyzeRep(replicate = 1,
                          analysisCode = dummyAnalysisCode,
                          doseCol = "DOSE, DOSES"),
               msg = "check the dose, must be of length one")

  expect_error(analyzeRep(replicate = 1,
                          analysisCode = dummyAnalysisCode,
                          doseCol = "-97DOSE"),
               msg = "check the dose, must be valid name")

  # test the argument of workingPath
  expect_error(analyzeRep(replicate = 2,
                          analysisCode = dummyAnalysisCode,
                          workingPath = analyseRep.datapath),
               msg = "wrong sort of input file, not same number of fields")

  expect_error(analyzeRep(replicate = 3,
                          analysisCode = dummyAnalysisCode,
                          workingPath = analyseRep.datapath),
               msg = "wrong sort of input file, does not have a DOSE column")

  # test the argument of parOmitFlag
  expect_error(analyzeRep(replicate = 1,
                          analysisCode = dummyAnalysisCode,
                          workingPath = analyseRep.datapath,
                          parOmitFlag = "PAROMIT,PARMOIT2"),
               msg = "wrong sort of paromit flag, too long")

  expect_error(analyzeRep(replicate = 1,
                          analysisCode = dummyAnalysisCode,
                          workingPath = analyseRep.datapath,
                          parOmitFlag = "@954fgPAROMIT"),
               msg = "wrong sort of paromit flag, wrong name")

  # test the argument of respOmitFlag
  expect_error(analyzeRep(replicate = 1,
                          analysisCode = dummyAnalysisCode,
                          workingPath = analyseRep.datapath,
                          respOmitFlag = "RESPOMIT,RESPMOIT2"),
               msg = "wrong sort of respomit flag, too long")

  expect_error(analyzeRep(replicate = 1,
                          analysisCode = dummyAnalysisCode,
                          workingPath = analyseRep.datapath,
                          respOmitFlag = "@954fgRESPOMIT" ),
               msg = "wrong sort of respomit flag, wrong name")

  #
})

