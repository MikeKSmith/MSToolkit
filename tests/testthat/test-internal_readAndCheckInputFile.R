if(!exists("unitTestPath")) unitTestPath <- system.file(package = "MSToolkit", "tests")
testDir <- file.path(unitTestPath, "testthat", "testdata.datastorage")

# SF issue 6
# Tue Jul 24 12:35:40 BST 2007 @524 /Internet Time/
test_that("test.readnonmemdata.sf6", {
  if("MSToolkit" %in% search()){ # need that because .readAndCheckInputFile is internal
    readFun <- MSToolkit:::.readAndCheckInputFile

    nohData <- readFun(file.path(unitTestPath, "testdata.datastorage", "NONMEMDataNoHeader.fit"))
    heaData <- readFun(file.path(unitTestPath, "testdata.datastorage", "NONMEMDataWithHeader.fit"))
    csvData <- readFun(file.path(unitTestPath, "testdata.datastorage", "NONMEMcsv.csv"))

    expect_equal(heaData, nohData, info = "check import of NONMEM data (1)")
    expect_equal(csvData, nohData, info = "check import of NONMEM data (2)")

    expect_equal(10, nrow(nohData), info = "check import of NONMEM data (3)")
    expect_equal(10, nrow(heaData), info = "check import of NONMEM data (4)")
    expect_equal(10, nrow(csvData), info = "check import of NONMEM data (5)")
  }
})
