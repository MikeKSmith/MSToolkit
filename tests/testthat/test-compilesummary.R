setEctdDataMethod("CSV")
if( !exists("unitTestPath")) unitTestPath <- system.file(package = "MSToolkit", "tests")
compileSummary.datapath <- file.path(unitTestPath, "testthat", "data", "compileSummary")
compileSummary2.datapath <- file.path(unitTestPath, "testthat", "data", "compileSummary2")
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
