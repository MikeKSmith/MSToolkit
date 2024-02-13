library(MSToolkit)

if( !exists("testPath")) testPath <- system.file(package = "MSToolkit", "tests")
testdata.supportfunctions.dir <- file.path( testPath,
                                            "teststhat",
                                            "testdata.supportfunctions" )

# .data.frame.compare <- function(
#   frame1,
#   frame2
# )
# {
#   ###############################################################################
#   # Mango Solutions, Chippenham SN14 0SQ 2006
#   # Sunday July 1 2007
#   # Author: Francisco Gochez
#   ###############################################################################
#   # DESCRIPTION: Compares two data frames that may have NA or NULL entries
#   # KEYWORDS:
#   ###############################################################################
#
#   if(any(!is.data.frame(frame1) || !is.data.frame(frame2)))
#     stop("Non-data frame input")
#   if(dim(frame1) != dim(frame2))
#     return(FALSE)
#   if(names(frame1) != names(frame2))
#     return(FALSE)
#   #if(length((!is.na(frame1) & !is.null(frame1))) != length(!is.na(frame2) & !is.null(frame2)))
#   #  return(FALSE)
#
#   # Condition 1: The non-NULL and non-NA entries of both data frames must be equal
#   cond1 <- all(frame1[(!is.na(frame1) & !is.null(frame1))] == frame2[(!is.na(frame2) & !is.null(frame2))])
#   # condition 2: The NA entries of both data frames must occur in the same places
#   cond2 <- all(is.na(frame1) == is.na(frame2))
#   # condition 3: The NULL entries of both data frames must occur in the same places
#   cond3 <- all(is.null(frame1) == is.null(frame2))
#
#   cond1 && cond2 && cond3
#
# }

test_that("test.checkSymmetricPDMatrix", {
  # Author: Francisco Gochez
  # Date: Jun 26 2007
  # Updated: June 24 2021 by Mike K Smith

    # check matrix filled with NA values
  testMatrix <- matrix(ncol=2, nrow=2)
  expect_error(checkSymmetricPDMatrix(testMatrix), regexp = "Missing values")

  #PD matrix
  A <- matrix( c( 2, -1, 0, -1, 2, -1, 0, -1, 2 ), nrow=3, byrow=TRUE )
  expect_null(checkSymmetricPDMatrix(A))

  B <- matrix( c( 2, -1, 2, -1, 2, -1, 2, -1, 2 ), nrow=3, byrow=TRUE )
  expect_null(checkSymmetricPDMatrix(B))

  # Not PD
  C <- matrix( c( -2, 1, 0, 1, -2, 1, 0, 1, -2 ), nrow=3, byrow=TRUE )
  expect_error(checkSymmetricPDMatrix(C), regexp = "not positive definite")

  E <- matrix( c( 1, 2, 0, 2, 1, 2, 0, 2, 1 ), nrow=3, byrow=TRUE )
  expect_error(checkSymmetricPDMatrix(E), regexp = "not positive definite")

  #Check for handling of non-square matrices
  expect_error(checkSymmetricPDMatrix(matrix(1, ncol =2, nrow = 3)),
               regexp = "not square")
})

test_that("test.initialChar",{
  # Author: Francisco Gochez Gochez
  # Date: Jun 26 2007

  expect_equal(initialChar("23524A"), "a")
  expect_error(initialChar("123"))
  expect_equal(initialChar("123b456d"), "b")
  expect_error(initialChar("123a456b", adm = "BCD"))
  expect_equal(initialChar("34Zy", adm="werZ"), "z")
  expect_error(initialChar(""))
})

test_that("test.ectdStop", {
  expect_error(ectdStop("test"))
  expect_error(ectdStop(NULL))
  # expect_output(ectdStop("test", verbose=TRUE, "test"))
})

test_that("test.parseCharInput",{
  expect_equal(parseCharInput("100,200,300"), c(100, 200, 300))
  expect_error(parseCharInput("1", expected = 2))
  expect_equal(parseCharInput("1, 2, 3", convertToNumeric = FALSE),
               c("1", "2", "3"))
  expect_error(parseCharInput("1,1", checkdup = TRUE))
  expect_error(parseCharInput("1,1", expected = 3))
  expect_error(parseCharInput("1,4", checkProb = TRUE))
})

# Author: Romain Francois

test_that("test.parseCovMatrix", {
  # wrong dimensions
  expect_error( parseCovMatrix("1,1,1,2", 2), regexp = "Dimension Problem" )
  # Not PD
  expect_error( parseCovMatrix("0,1,0", 2), regexp = "not positive definite" )

  #PD matrix
  A <- matrix( c( 2, -1, 0, -1, 2, -1, 0, -1, 2 ), nrow=3, byrow=TRUE )
  expect_equal(parseCovMatrix(A, nCov=3), A)

  B <- matrix( c( 2, -1, 2, -1, 2, -1, 2, -1, 2 ), nrow=3, byrow=TRUE )
  expect_equal(parseCovMatrix(B, nCov=3), B)

  # Not PD
  C <- matrix( c( -2, 1, 0, 1, -2, 1, 0, 1, -2 ), nrow=3, byrow=TRUE )
  expect_error(parseCovMatrix(C, nCov=3), regexp = "not positive definite")

  E <- matrix( c( 1, 2, 0, 2, 1, 2, 0, 2, 1 ), nrow=3, byrow=TRUE )
  expect_error(parseCovMatrix(E, nCov=3), regexp = "not positive definite")

  mat <- parseCovMatrix("1,0,1,.5,0,1", 3)
  mat2 <- rbind( c(1 , 0, .5),
                 c(0 , 1, 0 ),
                 c(.5, 0, 1 ) )
  expect_true(all(mat == t(mat)) )
  expect_equal(mat, mat2)

})

test_that("test.parseRangeCode",{
  expect_equal(as.character(parseRangeCode("x<4")), "(x < 4)")
  expr <- as.character(parseRangeCode(c("2 <= 10 >= x", "x >= x", "z < 4 < 1")))
  # Concatenate any output seperated by deparse and remove all whitespace
  expr <- gsub( "[[:space:]]", "", expr )
  expect_equal(expr, "(2<=10)&(10>=x)&(x>=x)&(z<4)&(4<1)")

  # Extra bracket where it is not needed
  expect_error(parseRangeCode(c("x < 4", "(2 > z")), regexp = "parsing problem")

  # No comparator
  expect_error(parseRangeCode(c("1 > z >= 4", "y")), regexp = "No comparator")

  # Check white space is handled
  expr <- as.character(parseRangeCode(
    c("1 >=         x       <         z", "1       <                2")))
  expr <- gsub(  "[[:space:]]", "", paste(expr, collapse="") )
  expect_equal(expr, "(1>=x)&(x<z)&(1<2)")

  # Check "|" not allowed
  expect_error(parseRangeCode("x < 4 | 2 > z"), regexp = "`|` not allowed")

  # Too many comparators
  expect_error(parseRangeCode("1< x < 4 < 9"), regexp = "Too many comparators")
})

test_that("test.parseHashString",{
  expect_error(parseHashString("2#A"))
  expect_error(parseHashString("1,2,3#2,;,4"))
  # Two consecutive commas not allowed
  expect_error(parseHashString(parseHashString("1,2,3,4#5,,6")),
               regexp = "comma")
  expect_equal(list(c(1,2,3)), parseHashString("1,2,3"))
  expect_equal(list(c(0), c(5,5,5,5,5,5,5,5), c(4)),
               parseHashString("0#5,5,5,5,5,5,5,5#4"))
  expect_error(parseHashString())
  expect_equal(parseHashString("1,2#3,4#5,6"),
               list(c(1,2),c(3,4),c(5,6)))
})

test_that("test.validNames",{
  expect_error(validNames(".2"), regexp = "\\.2")
  expect_error(validNames("foo", "bar", "5ar"), regexp = "5ar")
  expect_error(validNames("barof$oap"), regexp = "barof\\$oap")
})

test_that("test.checkColNames", {
  expect_error(checkColNames(letters, ".2"), regexp = "\\.2")
  expect_error(checkColNames(letters,c("foo", "bar", "5ar")), regexp = "5ar")
  expect_error(checkColNames(letters, "barof$oap"), regexp = "barof\\$oap")
  expect_error(checkColNames(letters, c("hello", "there")),
               regexp = "columns missing")
  expect_true(checkColNames(letters, letters))
})

test_that("test.parseRCode", {
  expect_true(is.expression(parseRCode("Hello")))
  expect_true(is.expression(parseRCode(1:5)))
  expect_true(is.expression(parseRCode(mtcars)))

  ## Check hygiene of this test
  suppressWarnings(rm(helloTestObject))
  expect_error(parseRCode(helloTestObject))
  })

test_that("test.checkMicroFormat",{
  expect_error(checkMicroFormat(1:5), regexp = "must be a data frame")
  expect_error(checkMicroFormat(data.frame()), regexp = "must be a data frame")
  expect_error(checkMicroFormat(data.frame(DOSE = 1:5), "Hello", TRUE),
               regexp = "must contain a 'DOSE' column")

  x <- data.frame(DOSE1 = c(0, 15, 30), dose2 = c(0, 15, 30), dOsE3 = c(0, 15, 30))
  d1 <- checkMicroFormat(x, "dose1", TRUE)$dose1
  d2 <- checkMicroFormat(x, "DOSE2", TRUE)$DOSE2
  d3 <- checkMicroFormat(x, "DoSe3", TRUE)$DoSe3

  expect_true( all(d1 == c(0, 15, 30)))
  expect_true( all(d2 == c(0, 15, 30)))
  expect_true( all(d3 == c(0, 15, 30)))
  })

test_that("test.dataAggregate",{
  N <- 200
  myDf <- data.frame(S1 = sample(1:3, N, T), S2 = sample(1:2, N, T), S3 = rep(1, N),
                     B1 = sample(1:N), B2 = sample(1:N, N, T), RESP = rnorm(N))

  out1 <- MSToolkit:::.dataAggregate(list(MEAN = myDf$RESP), myDf[c("S1", "S2")], mean, bound = 100)
  orig1 <- aggregate(list(MEAN = myDf$RESP), myDf[c("S1", "S2")], mean)
  expect_true(identical(out1, orig1))

  out2 <- MSToolkit:::.dataAggregate(list(MEAN = myDf$RESP), myDf[c("S1", "B1", "S2", "B2")], mean, bound = 100)
  orig2 <- aggregate(list(MEAN = myDf$RESP), myDf[c("S1", "B1", "S2", "B2")], mean)
  expect_true(identical(out2, orig2))
})
