# added to comply with SF issue 7
# Tue Jul 24 10:24:35 BST 2007 @433 /Internet Time/

test_that("test.sf7", {
  # example data
  myData <- expand.grid(X = 1:2, Y = 1:2, Z = 1:2)

  # function example
  out1 <- createResponse(data = myData,
                         equation = function(data) with(data, X+Y+Z),
                         covariance = 1,
                         range = "RESP < 3",
                         seed = 9)

  # same example using a character representation
  out2 <- createResponse(data = myData,
                         equation = "X+Y+Z",
                         covariance = 1,
                         range = "RESP < 3",
                         seed = 9)
  expect_equal(out2,
               out1,
               info = "checking the function/character versions of createResponse")
})
