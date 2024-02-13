test_that("test.checkSimAlpha", {

  # Test proving multiple alphas (numeric)
  expect_error(checkSimAlpha(1:10), info = "Fail on multiple numeric inputs")
  expect_error(checkSimAlpha(letters), info = "Fail on multiple character inputs")

  # Character inputs
  expect_true(checkSimAlpha("95%") == .95, info = "Character with percent")
  expect_true(checkSimAlpha("9 5 % ") == .95, info = "Spaced character with percent")
  expect_true(checkSimAlpha(" 95 ") == .95, info = "Spaced character without percent")
  expect_error(checkSimAlpha(" hello "), info = "Invalid character input")

  # Check range of inputs (numeric)
  testVec <- seq(50, 100, by=.5)
  for (i in testVec) expect_true(checkSimAlpha(i) == i/100,
                                 info = paste("Check that '", i, "' parses correctly", sep=""))

  testVec <- seq(1.5, 50, by=.5)
  for (i in testVec) expect_true(checkSimAlpha(i) == (1-i/100),
                                 info = paste("Check that '", i, "' parses correctly", sep=""))

})
