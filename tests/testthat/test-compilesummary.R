setEctdDataMethod("CSV")
unitTestPath <- MSToolkit:::testthat_path
compileSummary.datapath <- file.path(unitTestPath, "data", "compileSummary")
compileSummary2.datapath <- file.path(unitTestPath, "data", "compileSummary2")
cat("compileSummary.datapath:", compileSummary.datapath, "\n")
cat("compileSummary2.datapath:", compileSummary.datapath, "\n")



test_that("test.compileSummary.failure", {

  expect_error(compileSummary("frew"),
               info = "Micro or Macro")

  dir.create( tf <- tempfile() )
  expect_error(compileSummary("Micro", workingPath = tf),
               info = "no MicroEvaluation directory")
  expect_error(compileSummary("Macro", workingPath = tf),
               info = "no MacroEvaluation directory")
  dir.create( file.path(tf, "MicroEvaluation"))
  dir.create( file.path(tf, "MacroEvaluation"))
  expect_error(compileSummary("Micro", workingPath = tf),
               info = "no csv files in MicroEvaluation directory")
  expect_error(compileSummary("Macro", workingPath = tf),
               info = "no csv files in MacroEvaluation directory" )

  unlink(tf, recursive = TRUE)
})


test_that("test.compileSummary.R", {

  ## copy files accross
  dir.create(cpdir <- file.path(tempdir(), "compileSummary.R"))
  dir.create(file.path(cpdir, "MacroEvaluation"))
  dir.create(file.path(cpdir, "MicroEvaluation"))
  for(i in 1:5){
    file.copy(file.path(compileSummary.datapath, "MicroEvaluation", sprintf("micro%04d.csv", i)),
              file.path(cpdir, "MicroEvaluation"))
    file.copy(file.path(compileSummary.datapath, "MacroEvaluation", sprintf("macro%04d.csv", i)),
              file.path(cpdir, "MacroEvaluation"))
  }

  compileSummary("Micro", workingPath = cpdir)
  compileSummary("Macro", workingPath = cpdir)
  expectedMicroData <- read.csv(file.path(compileSummary.datapath, "microSummary.csv"))
  newMicroData <- read.csv(file.path(compileSummary.datapath, "microSummary.csv"))
  expect_equal(newMicroData, expectedMicroData,
               info = "compile micro R")
  expectedMacroData <- read.csv(file.path(compileSummary.datapath, "macroSummary.csv"))
  newMacroData <- read.csv(file.path(compileSummary.datapath, "macroSummary.csv"))
  expect_equal(newMicroData, expectedMicroData,
               info = "compile macro R")

  unlink(cpdir, recursive = TRUE)

})


test_that("test.compileSummary.missing", {

  ## copy files accross
  dir.create(cpdir <- file.path(tempdir(), "compileSummary.missing"))
  dir.create(file.path(cpdir, "MacroEvaluation"))
  dir.create(file.path(cpdir, "MicroEvaluation"))
  for(i in 1:5){
    file.copy(file.path(compileSummary.datapath, "MicroEvaluation", sprintf("micro%04d.csv", i)),
              file.path(cpdir, "MicroEvaluation"))
    file.copy(file.path(compileSummary.datapath, "MacroEvaluation", sprintf("macro%04d.csv", i)),
              file.path(cpdir, "MacroEvaluation"))
  }

  expect_error(compileSummary("Micro", replicates = 1:10, workingPath = cpdir),
               info = "not compile when missing files (micro)")
  expect_error(compileSummary("Macro", replicates = 1:10, workingPath = cpdir),
               info = "not compile when missing files (macro)")

  unlink(cpdir, recursive = TRUE)

})


test_that("test.compileSummary.holes.R", {

  ## copy files accross
  dir.create(cpdir <- file.path(tempdir(), "compileSummary.missing"))
  dir.create(file.path(cpdir, "MacroEvaluation"))
  dir.create(file.path(cpdir, "MicroEvaluation"))
  for(i in 1:5){
    file.copy(file.path(compileSummary2.datapath, "MicroEvaluation", sprintf("micro%04d.csv", i)),
              file.path(cpdir, "MicroEvaluation"))
    file.copy(file.path(compileSummary2.datapath, "MacroEvaluation", sprintf("macro%04d.csv", i)),
              file.path(cpdir, "MacroEvaluation"))
  }

  expect_error(compileSummary("Micro", workingPath = cpdir),
               info = "not all columns the same")
  expect_error(compileSummary("Macro", workingPath = cpdir),
               info = "not all columns the same")

  unlink(cpdir, recursive = TRUE)

})