
if( !exists("unitTestPath")) unitTestPath <- system.file(package = "MSToolkit", "Runit")
testdata.supportfunctions.dir <- file.path( unitTestPath, "testdata.supportfunctions" )

.data.frame.compare <- function(
  frame1,
  frame2
)
{
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # Sunday July 1 2007
  # Author: Francisco
  ###############################################################################
  # DESCRIPTION: Compares two data frames that may have NA or NULL entries
  # KEYWORDS:
  ###############################################################################

  if(any(!is.data.frame(frame1) || !is.data.frame(frame2)))
    stop("Non-data frame input")
  if(dim(frame1) != dim(frame2))
    return(FALSE)
  if(names(frame1) != names(frame2))
    return(FALSE)
  #if(length((!is.na(frame1) & !is.null(frame1))) != length(!is.na(frame2) & !is.null(frame2)))
  #  return(FALSE)

  # Condition 1: The non-NULL and non-NA entries of both data frames must be equal
  cond1 <- all(frame1[(!is.na(frame1) & !is.null(frame1))] == frame2[(!is.na(frame2) & !is.null(frame2))])
  # condition 2: The NA entries of both data frames must occur in the same places
  cond2 <- all(is.na(frame1) == is.na(frame2))
  # condition 3: The NULL entries of both data frames must occur in the same places
  cond3 <- all(is.null(frame1) == is.null(frame2))

  cond1 && cond2 && cond3

}


# Author: Francisco
 # Date: Jun 26 2007


 test.checkSymmetricPDMatrix <- function()
 {

  # check matrix filled with NA values
  testMatrix <- matrix(ncol=2, nrow=2)
  checkException(checkSymmetricPDMatrix(testMatrix))
  # testMatrix <- diag(100)
  #checkException(checkSymmetricPDMatrix(testMatrix))

  # Check negative-definite matrix
  checkException(checkSymmetricPDMatrix(-1 * testMatrix))
  testMatrix[1,2] <- 0.1
  checkException(checkSymmetricPDMatrix(testMatrix))

  #Check for handling of non-square matrices
  checkException(checkSymmetricPDMatrix(matrix(1, ncol =2, nrow = 3)))
  # Check 0 matrix
  #< DOES NOT FAIL ANYMORE SINCE VERSION 0.5: testMatrix <- matrix(0, ncol = 2, nrow = 2)
  #< DOES NOT FAIL ANYMORE SINCE VERSION 0.5: checkException(checkSymmetricPDMatrix(testMatrix))
  #testMatrix <- matrix(1, ncol = 1, nrow = 1)
  #checkException(checkSymmetricPDMatrix(testMatrix))

 }

 # Author: Francisco
 # Date: Jun 26 2007

 test.initialChar <- function()
 {
  #cat("Testing initialChar... \n")
  checkEquals(initialChar("23524A"), "a")
  #<T> No letters present, should generate exception
  checkException(initialChar("123"))
  #<T> First letter is a lower case "b"
  checkEquals(initialChar("123b456d"), "b")
  checkException(initialChar("123a456b", adm = "BCD"))
  checkEquals(initialChar("34Zy", adm="werZ"), "z")
  checkException(initialChar(""))
 }

 test.ectdStop <- function()
 {
  checkException(ectdStop("test"))
  # checkException(etcdStop(NULL))
 }

 test.ectdWarning <- function()
 {
  # checkException(ectdWarning(NULL))
 }

 test.parseCharInput <- function()
 {
  checkEquals(parseCharInput("100,200,300"), c(100, 200, 300))
  checkException(parseCharInput("1", expectedLength = 2))
  checkEquals(parseCharInput("1, 2, 3", convertToNumeric = FALSE), c("1", "2", "3"))

  checkException(parseCharInput("1,1", checkdup = TRUE))
  checkException(parseCharInput("1,1", expectedLength = 3))
  checkException(parseCharInput("1,4", checkProb = TRUE))

 }

# Author: Romain
test.parseCovMatrix <- function(){

  # wrong dimensions
  checkException( parseCovMatrix("1,1,1,2", 2) )

  # not PD
  checkException( parseCovMatrix("0,1,0", 2) )

  mat <- parseCovMatrix("1,0,1,.5,0,1", 3)
  mat2 <- rbind( c(1 , 0, .5),
                 c(0 , 1, 0 ),
                 c(.5, 0, 1 ) )

  checkTrue( all(mat == t(mat)) )
  checkEquals( mat, mat2 ,
    msg = "Testing that the matrix is generated a la NONMEM")
}

test.parseRangeCode <- function()
{
	checkEquals(as.character(parseRangeCode("x<4")), "(x < 4)")
	expr <- as.character(parseRangeCode(c("2 <= 10 >= x", "x >= x", "z < 4 < 1")))

	# Concatenate any output seperated by deparse and remove all whitespace
	expr <- gsub( "[[:space:]]", "", expr )
	checkEquals(expr, "(2<=10)&(10>=x)&(x>=x)&(z<4)&(4<1)")
	checkException(parseRangeCode(c("x < 4", "(2 > z")))
	checkException(parseRangeCode(c("1 > z >= 4", "y")))

	expr <- as.character(parseRangeCode(c("1 >=               x         <             z", "1         <                2")))
	expr <- gsub(  "[[:space:]]", "", paste(expr, collapse="") )
	checkEquals(expr, "(1>=x)&(x<z)&(1<2)")
}

test.parseHashString <- function()
{
  checkException(parseHashString("2#A"))
  checkException(parseHashString("1,2,3#2,;,4"))
  checkException(parseHashString(parseHashString("1,2,3,4#5,,6"), msg = "Two consecutive commas are not allowed"))
  checkEquals(list(c(1,2,3)), parseHashString("1,2,3"))
  checkEquals(list(c(0), c(5,5,5,5,5,5,5,5), c(4)), parseHashString("0#5,5,5,5,5,5,5,5#4"))
  checkException(parseHashString())
  #checkEquals(parseHashString("1,2#3,4#5,6"), c(1,2,3,4,5,6))
}

test.validNames <- function()
{
  checkException(validNames(".2"), msg = "Can't follow initial period with a number")
  checkException(validNames("foo", "bar", "5ar"), "Variable names cannot start with a number")
  checkException(validNames("barof$oap"), "$ is not allowed in a variable name")
}

test.checkColNames <- function() {
	checkException(checkColNames(letters, ".2"), msg = "Calling through to validNames")
	checkException(checkColNames(letters, c("foo", "bar", "5ar")), "Calling through to validNames")
	checkException(checkColNames(letters, "barof$oap"), "Calling through to validNames")
	checkException(checkColNames(letters, c("hello", "there")))
	checkTrue(checkColNames(letters, letters))
}

test.parseRCode <- function() {
	checkTrue(is.expression(parseRCode("Hello")))
	checkTrue(is.expression(parseRCode(1:5)))
	checkTrue(is.expression(parseRCode(mtcars)))
	suppressWarnings(rm(helloTestObject))
	checkException(parseRCode(helloTestObject))
}

# Author: Rich P
# Date: December 14
test.checkMicroFormat <- function()
{
	checkException( checkMicroFormat(1:5), msg = "Not a data frame")
	checkException( checkMicroFormat(data.frame()), msg = "Not a data frame")
	checkException( checkMicroFormat(data.frame(DOSE = 1:5), "Hello", TRUE), msg = "Missing dose variable")

	x <- data.frame(DOSE1 = c(0, 15, 30), dose2 = c(0, 15, 30), dOsE3 = c(0, 15, 30))
	d1 <- checkMicroFormat(x, "dose1", TRUE)$dose1
	d2 <- checkMicroFormat(x, "DOSE2", TRUE)$DOSE2
	d3 <- checkMicroFormat(x, "DoSe3", TRUE)$DoSe3
	checkTrue( all(d1 == c(0, 15, 30)), msg = "Dose matching on case: example 1")
	checkTrue( all(d2 == c(0, 15, 30)), msg = "Dose matching on case: example 2")
	checkTrue( all(d3 == c(0, 15, 30)), msg = "Dose matching on case: example 3")

}

test.dataAggregate <- function() {

	N <- 200
	myDf <- data.frame(S1 = sample(1:3, N, T), S2 = sample(1:2, N, T), S3 = rep(1, N),
			B1 = sample(1:N), B2 = sample(1:N, N, T), RESP = rnorm(N))

	out1 <- MSToolkit:::.dataAggregate(list(MEAN = myDf$RESP), myDf[c("S1", "S2")], mean, bound = 100)
	orig1 <- aggregate(list(MEAN = myDf$RESP), myDf[c("S1", "S2")], mean)
	checkTrue(identical(out1, orig1))

	out2 <- MSToolkit:::.dataAggregate(list(MEAN = myDf$RESP), myDf[c("S1", "B1", "S2", "B2")], mean, bound = 100)
	orig2 <- aggregate(list(MEAN = myDf$RESP), myDf[c("S1", "B1", "S2", "B2")], mean)
	checkTrue(identical(out2, orig2))

}



