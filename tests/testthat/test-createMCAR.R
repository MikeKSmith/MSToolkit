test_that("test.data.missing.mcar", {

  tdata <- data.frame(SUBJ = rep(1:3, each = 3),
                      TIME = rep(0:2, 3))

  expect_true(all(createMCAR(tdata, prop = 0, flagName = "ABC")$ABC == 0))
  expect_true(all(createMCAR(tdata, prop = 1)$MISSING == 1))

  expect_error(createMCAR(tdata, prop = 100))
  expect_error(createMCAR(tdata, prop = -10))

  out <- createMCAR(tdata, prop = 1, rule = "TIME > 0")
  expect_true(all(out$MISSING[ tdata$TIME == 0 ] == 0))
  expect_true(all( out$MISSING[ tdata$TIME >  0 ] == 1))

  expect_error(createMCAR(tdata, prop = 1, rule = "NOTEXISTS > 0"))

  expect_error(createMCAR(tdata, prop = 1, rule = "TIME > 10"))

  expect_error(createMCAR(tdata, prop = 1, flagName = "0e321"))

  tdata <- expand.grid(SUBJ = 1:100, TIME = 0:4)
  tdata$MISSING <- sample(c(0,1) , size = 500, replace = TRUE)
  out <- createMCAR(tdata, prop = .5)
  expect_true(all(out$MISSING[tdata$MISSING == 1] == 1))
})