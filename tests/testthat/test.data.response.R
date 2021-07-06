test_that("test.data.response.addResidualError", {
    resp <- rep(0, 100)
    expect_error(addResidualError(resp), 
                 regexp = "The 'covariance' argument is required")
    expect_error(addResidualError(covariance = "1"), 
                 regexp = "The 'response' variable is required")
    expect_error(addResidualError(), 
                 regexp = "The 'response' variable is required")
    
    # expect_error(addResidualError(resp, "1", errStruc = "q"))
    
    myf <- function(x) log(x)
    expect_error(addResidualError(resp, "1", errStruc = myf))
    myf <- function(x, y) tail((x + y))
    expect_error(addResidualError(resp, "1", errStruc = myf))
    r1 <- addResidualError(resp, "1", errStruc = "A", seed = 1)
    junk <- rnorm(100) + runif(100)
    r2 <- addResidualError(resp, "1", errStruc = "A", seed = 1)
    expect_true(all(r1 == r2))
    
    expect_error(addResidualError(resp, "1", errStruc = "C", seed = 1),
                  regexp = "Combined error structure must use bivariate")
    expect_error(addResidualError(seq(1,10), diag(c(1,1)), errStruc = "A"),
                 regexp = "must use univariate covariance")   
})

# test_that("test.data.response.createResponse", {
#     expect_equal(mtcars$mpg + mtcars$cyl, createResponse(mtcars, 
#         "mpg+cyl")[, 1])
#     expect_true(all(createResponse(mtcars, "mpg+cyl")$RESPOMIT == 
#         0))
#     expect_error(createResponse(mtcars, "mpg+cyl", distribution = "gre"))
#     expect_error(createResponse(mtcars, "mpg+cyl", name = "'fea"))
#     expect_error(createResponse(mtcars, "mpg+cyl", flagName = "-9ea"))
#     expect_error(createResponse(mtcars, "mpg+cyl", flagName = "XX", 
#         name = "XX"))
#     expect_error(createResponse(mtcars, "mpg+cyl", digits = "-1"))
#     expect_error(createResponse(mtcars, "mpg+cyl", digits = "1,2"))
#     expect_error(createResponse(mtcars, "mpg+cyl", range = "mpg"))
#     resp <- createResponse(mtcars, "mpg", range = "mpg < 4")
#     expect_true(all(resp$PAROMIT[mtcars$mpg < 4] == 1))
#     resp <- createResponse(mtcars, "mpg", range = "RESP < 4")
#     expect_true(all(resp$PAROMIT[mtcars$mpg < 4] == 1))
#     expect_equal(c(nrow(mtcars), 2), dim(resp))
# })
# 
# test_that("test.data.response.createResponseVariable", {
#     expect_error(createResponseVariable(equation = "X+Z"))
#     expect_error(createResponseVariable(data = list(X = 1, Z = 3), 
#         "X+Z"))
#     expect_true(all(createResponseVariable(mtcars, "cyl+disp") == 
#         mtcars$cyl + mtcars$disp))
#     expect_equal(nrow(mtcars), length(createResponseVariable(mtcars, 
#         "cyl+disp")))
#     expect_error(createResponseVariable(mtcars, "disp[1:50]"))
#     expect_error(createResponseVariable(mtcars, "rnorm(10)"))
#     expect_error(createResponseVariable(mtcars, "Sepal.Length+Species"))
#     expect_error(createResponseVariable(mtcars, "{"))
#     preFalse <- try(createResponseVariable(mtcars, "cyl+", preTest = FALSE), 
#         silent = TRUE)[1]
#     preTrue <- try(createResponseVariable(mtcars, "cyl+", preTest = TRUE), 
#         silent = TRUE)[1]
#     expect_true(length(grep("subset", preFalse)) == 0)
#     expect_true(length(grep("subset", preTrue)) > 0)
# })
# 
# test_that("test.data.response.dropbug", {
#     myData <- data.frame(X = rnorm(10))
#     out <- try(createResponse(myData, "X"))
#     expect_true(class(out) != "try-error", info = "drop = FALSE bugfix")
#     expect_equal(round(myData$X, 3), out$RESP, info = "drop = FALSE bugfix sanity check")
# })
# 
# test_that("test.data.response.sf10", {
#     myData <- expand.grid(X = 1:2, Y = 1:2, Z = 1:2)[rep(1:6, 
#         10000), ]
#     expect_error(createResponse(data = myData, equation = "X+Y+Z", 
#         invLink = function(x) rep(1.3, length(x)), distribution = "b"), 
#         info = "invLink should generate probabilities")
#     out <- createResponse(data = myData, equation = "X+Y+Z", 
#         invLink = function(x) rep(0.9, length(x)), distribution = "b")$RESP
#     expect_true(binom.test(table(out), p = 0.1, alternative = "two.sided")$p.value > 
#         0.05, info = "stats test for the binomial response where invLink is given")
#     out <- createResponse(data = myData, equation = "X+Y+Z", 
#         invLink = function(x) rep(1, length(x)), distribution = "b")$RESP
#     expect_true(all(out == 1), info = "stats test for the binomial response where invLink is given (1)")
#     out <- createResponse(data = myData, equation = "X+Y+Z", 
#         invLink = function(x) rep(0, length(x)), distribution = "b")$RESP
#     expect_true(all(out == 0), info = "stats test for the binomial response where invLink is given (0)")
# })
# 
