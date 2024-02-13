test_that("test.data.checkDropuOutFun", {

  dFun <- function(data, prop) sample(0:1, nrow(data), TRUE, c(1-prop, prop))
  testData <- data.frame(SUBJ=rep(1:10, each=5),
                         TIME=rep(0:4, 10),
                         VALUE=rnorm(50))
  doData <- createDropout(testData, dFun, prop=0.4, seed=10)

  # tests about the drop out function
  expect_error(checkDropOutFun(max), info = "wrong function")
  expect_error(checkDropOutFun("ghost"), info = "does not exist")
  expect_error(checkDropOutFun(ghost), info = "does not exist")
  expect_error(checkDropOutFun(function(data, prop)  1:nrow(data), testData),
               info = "wrong function, does not generate 0,1,T,F")
  expect_error(checkDropOutFun(function(data, prop)  stop("error"), doData),
               info = "error occuring")
  expect_true(checkDropOutFun(function(data, prop)  rep(1, nrow(data)), testData))
  expect_error(checkDropOutFun(function(dta, prop)  rep(1, nrow(data)), testData))
  dFun <- function(data, ...) rep(1, 5)
  expect_true(checkDropOutFun(dFun, testData))
  expect_error(checkDropOutFun(dFun, testData, useSubset = FALSE))
  expect_error(checkDropOutFun(dFun, testData, sizeSubset = 6))
})