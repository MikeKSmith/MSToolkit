test_that("test.data.missing.drop", {

  # test with a correct dropout function
  dFun <- function(data, prop) sample(0:1, nrow(data), TRUE, c(1-prop, prop))
  testData <- data.frame(SUBJ=rep(1:10, each=5),
                         TIME=rep(0:4, 10),
                         VALUE=rnorm(50))

  doData <- createDropout(testData, dFun, prop=0.4, seed=10)

  out <- with(doData, tapply(MISSING, SUBJ, function(x) {
    w1 <- which(x == 1)
    length(w1) == 0 || all(w1 == seq(to = 5, length.out = length(w1)))
  }))
  expect_true(all(out) ,
              info = "check that the dropout is retained until the end")

  # tests about the drop out function
  expect_error(createDropout(doData, max),
               info = "wrong function")

  expect_error(createDropout(doData, function(data, prop) 1:nrow(data)),
               info = "wrong function, does not generate 0,1,T,F")

  expect_error(createDropout(doData, function(data, prop) stop("error")),
               info = "error occuring")

  expect_error(createDropout(doData, "thisfunctiondoesnotexist"),
               info = "function not existing")


  # test about invalid names
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