test_that("test.checkMacroFormat", {

  expect_error(checkMacroFormat(1:10),
               info = "not a data frame")

  expect_error(checkMacroFormat(iris[1:10, ]),
               info = "more than one row")

  expect_true(checkMacroFormat(data.frame(x =1 )))
})
