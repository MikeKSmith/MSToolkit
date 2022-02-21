setEctdDataMethod("CSV")

if( !exists("unitTestPath")) unitTestPath <- system.file(package = "MSToolkit", "tests")
analyseRep.datapath <- file.path( unitTestPath , "testthat", "data", "analyseRep" )
cat("analyseRep.datapath:", analyseRep.datapath, "\n" )

test_that("test_analyzeRep_rep", {
  # test the argument of replicate
  expect_error(analyzeRep(replicate = 2:7),
               info = "check the handling of the replicate arg, must be of length one")
  expect_error(analyzeRep(replicate = -3),
               info = "check the handling of the replicate arg, must be positive")
  expect_error(analyzeRep(replicate = "notanumber"),
               info = "check the handling of the replicate arg, must be a number")

  wrongfun <- function(notdata){
    notdata
  }
  # test the argument of analysisCode
  expect_error(analyzeRep(replicate = 1,
                          analysisCode = wrongfun),
               info = "analysis code must have a data argument")
  expect_error(analyzeRep( replicate = 1,
                           analysisCode = "doesnotexists"),
               info = "analysis code must be available")

  dummyAnalysisCode <- function(data){data}

  # test the argument of doseCol
  expect_error(analyzeRep(replicate = 1,
                          analysisCode = dummyAnalysisCode,
                          doseCol = "DOSE, DOSES"),
               info = "check the dose, must be of length one")

  expect_error(analyzeRep(replicate = 1,
                          analysisCode = dummyAnalysisCode,
                          doseCol = "-97DOSE"),
               info = "check the dose, must be valid name")

  # test the argument of workingPath
  expect_error(analyzeRep(replicate = 2,
                          analysisCode = dummyAnalysisCode,
                          workingPath = analyseRep.datapath),
               info = "wrong sort of input file, not same number of fields")

  expect_error(analyzeRep(replicate = 3,
                          analysisCode = dummyAnalysisCode,
                          workingPath = analyseRep.datapath),
               info = "wrong sort of input file, does not have a DOSE column")

  # test the argument of parOmitFlag
  expect_error(analyzeRep(replicate = 1,
                          analysisCode = dummyAnalysisCode,
                          workingPath = analyseRep.datapath,
                          parOmitFlag = "PAROMIT,PARMOIT2"),
               info = "wrong sort of paromit flag, too long")

  expect_error(analyzeRep(replicate = 1,
                          analysisCode = dummyAnalysisCode,
                          workingPath = analyseRep.datapath,
                          parOmitFlag = "@954fgPAROMIT"),
               info = "wrong sort of paromit flag, wrong name")

  # test the argument of respOmitFlag
  expect_error(analyzeRep(replicate = 1,
                          analysisCode = dummyAnalysisCode,
                          workingPath = analyseRep.datapath,
                          respOmitFlag = "RESPOMIT,RESPMOIT2"),
               info = "wrong sort of respomit flag, too long")

  expect_error(analyzeRep(replicate = 1,
                          analysisCode = dummyAnalysisCode,
                          workingPath = analyseRep.datapath,
                          respOmitFlag = "@954fgRESPOMIT" ),
               info = "wrong sort of respomit flag, wrong name")

  # test the argument of missomit flag
  expect_error(analyzeRep(replicate = 1,
                          analysisCode = dummyAnalysisCode,
                          workingPath = analyseRep.datapath,
                          missingFlag = "MISSING,MISSING2" ),
               info = "wrong sort of missomit flag, too long")

  checkException(analyzeRep(replicate = 1,
                            analysisCode = dummyAnalysisCode,
                            workingPath = analyseRep.datapath,
                            missingFlag = "@954fgMISSING" ),
                 info = "wrong sort of missomit flag, wrong name")

  # test the argument of interimCol
  expect_error(analyzeRep(replicate = 1,
                          analysisCode = dummyAnalysisCode,
                          workingPath = analyseRep.datapath,
                          interimCol = "INTERIM,INTERIM2" ),
               info = "wrong sort of interim flag, too long")

  expect_error(analyzeRep(replicate = 1,
                          analysisCode = dummyAnalysisCode,
                          workingPath = analyseRep.datapath,
                          interimCol = "@954fginterim" ),
               info = "wrong sort of imterim flag, wrong name")

  # generate the sample of interimCode
  interimCode <- function( data ){
    outList <- list()
    outList$STOP <- FALSE
    outList
  }

  # generate the sample of analysisCode
  anaCode <- function(data){
    with( data, {
      uniDoses <- sort( unique(DOSE))
      outDf <- data.frame( DOSE = uniDoses,
                           MEAN = tapply(RESP, DOSE, mean) ,
                           SE   = tapply(RESP, DOSE, sd )  )
      outDf$LOWER <- outDf$MEAN - 2 * outDf$SE
      outDf$UPPER <- outDf$MEAN + 2 * outDf$SE
      outDf$N     <- table(DOSE)[ as.character(uniDoses) ]
      outDf
    })
  }

  # test the correct names in the result
  res <- analyzeRep(replicate = 1,
                    analysisCode = anaCode,
                    interimCode = interimCode,
                    workingPath = analyseRep.datapath )
  expect_true(all(c("INTERIM","INTERIMC","DOSE","MEAN","SE","LOWER","UPPER","N","DROPPED","STOPPED") %in% names(res)),
             info = "check that the correct names are in the result" )

  # load the test data
  idata <- read.csv(file.path(analyseRep.datapath, "ReplicateData", "replicate0001.csv" ),
                    header = TRUE,
                    row.names = NULL)
  # test the interim column
  expect_equal(sort(unique(res$INTERIM)), c(0,sort(unique(idata$INTERIM))),
               info = "checking the interim column")

  # test the INTERIMC column
  expect_true(all(res$INTERIMC[ res$INTERIM == 0] == "FULL"),
              info = "checking the INTERIMC column, full")
  expect_true(all(res$INTERIMC[ res$INTERIM == max( res$INTERIM) ] == "FINAL"),
              info = "checking the INTERIMC column, final")

  subFin  <- subset( res , INTERIMC == "FINAL" )
  subFull <- subset( res , INTERIMC == "FINAL" )
  rownames(subFin) <- rownames(subFull) # to make identical happy
  expect_equal(subFull, subFin,
               info = "checking final and full analysis, must be similar when no changes")

  # test the number of subjects grows as the interim grows
  n <- with(subset(res, INTERIMC != "FULL"),
            do.call(cbind, tapply(N, INTERIM, function(x) x)))
  expect_true(all(apply(n, 1, diff) >= 0),
              info = "checking that the number of subjects grows as the interim grows")

  # test lower is lower than upper
  expect_true(all(res$LOWER <= res$UPPER),
              info = "check that lower is lower than upper")

})

