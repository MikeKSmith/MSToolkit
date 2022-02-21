test_that("test.data.typicalValue", {

  # Checking exceptions
  expect_error(createTvDefinition())
  expect_error(createTvDefinition("A"))
  expect_error(createTvDefinition("A", 1))
  expect_error(createTvDefinition("A", 1, 1:3, type = "X"))
  expect_error(createTvDefinition(letters, 1, 1:3))
  expect_error(createTvDefinition("A", 1:2, 1:3))
  expect_error(createTvDefinition("A", -1, 1:3))
  expect_error(createTvDefinition("A", 1, 1:3, type = "C"))
  expect_error(createTvDefinition("C", 3, 1:3, -1:5, "C", cbind(1:3, 1:3)))

  # Check "working" examples
  out1 <- createTvDefinition("A", 1, 1:3)
  out2 <- createTvDefinition("B", 2, 1:3, -1:5)
  out3 <- createTvDefinition("C", 3, 1:3, -1:5, "C", cbind(rep(1, 5), rep(2, 5)))

  # Now test outputs: first the basics
  expect_true(out1@Name == "A" & out1@Value == 1)
  expect_true(out2@Name == "B" & out2@Value == 2)
  expect_true(out3@Name == "C" & out3@Value == 3)

  # Now the eventual calls to treatment
  out1 <- out1@trtCall
  out2 <- out2@trtCall
  out3 <- out3@trtCall

  expect_true(all(out1$doses == 1:3)); expect_true(all(out2$doses == 1:3)); expect_true(all(out3$doses == 1:3))
  expect_true(is.null(out1$times)); expect_true(all(out2$times == -1:5)); expect_true(all(out3$times == -1:5))
  expect_true(out1$type == "Parallel"); expect_true(out2$type == "Parallel"); expect_true(out3$type == "Crossover")
  expect_true(identical(out3$sequence, cbind(rep(1, 5), 2)))
})
