#' Create Treatment Design Data
#'
#' Create treatment group for Parallel or Crossover design
#'
#' The function will first check for the required inputs.  If type is
#' "Parallel", then doses must be provided.  If type is "Crossover", the
#' sequence matrix must be provided.
#'
#' If the dose type is "Parallel" and there is are no specified "times", the
#' function will create a data frame with Treatment set to "1 to number of
#' doses" and a Dose variable with the doses specified.  If the dose type is
#' "Parallel" and there is a specified "times" vector, the function will create
#' a data frame with a parallel treatment regime for each dose specified.
#'
#' If the dose type is "Crossover", and there is are no specified "times"
#' input, the "times" input will be set to "1 to number of rows of the sequence
#' matrix" If "times" has been supplied, and has leading non-positive elements,
#' the sequence matrix is appended to a set of run-in measurements (where dose
#' is set to 0).  Based on the "times" and "sequence" matrix, a data frame is
#' created by aligning each column of the matrix with the times specified.
#'
#' @param doses (Required) Vector of doses to use.  Alternatively can be a
#' comma separated string of numbers
#' @param times (Optional) Vector of time points for dosing.  No time element
#' by default
#' @param type (Optional) Type of dosing regime to create: "Parallel" or
#' "Crossover".  "Parallel" by default.  See "details" section.
#' @param sequence (Optional) Crossover sequence matrix.  By default, no
#' crossover is performed.  See "details" section.
#' @param doseCol (Optional) Dose variable name to create ("DOSE" by default)
#' @param timeCol (Optional) Time variable name to create ("TIME" by default)
#' @param trtCol (Optional) Treatment variable name to create ("TRT" by
#' default)
#' @return A data frame containing a treatment, dose and (optionally) a time
#' variable
#' @author Roman Francois
#' @seealso \code{\link{allocateTreatments}}
#' @keywords datagen
#' @examples
#'
#'
#' createTreatments(doses = c(0, 15, 30))
#' #  TRT DOSE
#' # 1   1    0
#' # 2   2   15
#' # 3   3   30
#' createTreatments(doses = c(0, 15, 30), times = 0:2)
#' #  TRT TIME DOSE
#' # 1   1    0    0
#' # 2   1    1    0
#' # 3   1    2    0
#' # 4   2    0    0
#' # 5   2    1   15
#' # 6   2    2   15
#' # 7   3    0    0
#' # 8   3    1   30
#' # 9   3    2   30
#'
#' createTreatments(sequence = cbind(c(0, 15, 30), c(15, 30, 0), c(30, 0, 15)))
#' #  TRT TIME DOSE
#' # 1   1    1    0
#' # 2   1    2   15
#' # 3   1    3   30
#' # 4   2    1   15
#' # 5   2    2   30
#' # 6   2    3    0
#' # 7   3    1   30
#' # 8   3    2    0
#' # 9   3    3   15
#'
#' createTreatments(sequence = cbind(c(0, 15, 30),
#'                                   c(15, 30, 0),
#'                                   c(30, 0, 15)),
#'                                   times = 0:3)
#' #   TRT TIME DOSE
#' # 1    1    0    0
#' # 2    1    1    0
#' # 3    1    2   15
#' # 4    1    3   30
#' # 5    2    0    0
#' # 6    2    1   15
#' # 7    2    2   30
#' # 8    2    3    0
#' # 9    3    0    0
#' # 10   3    1   30
#' # 11   3    2    0
#' # 12   3    3   15
createTreatments <- function(
                             doses,
                             times = NULL,
                             type = "Parallel",
                             sequence,
                             doseCol = getEctdColName("Dose"),
                             timeCol = getEctdColName("Time"),
                             trtCol = getEctdColName("Trt")
) {
  ##############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # createTreatments.R Fri Jun 01 11:47:08 BST 2007 @491 /Internet Time/
  #
  # Author: Romain Francois
  ##############################################################################
  # DESCRIPTION: creates a data frame of all possible treatments for a given
  # scenario
  # KEYWORDS: datagen, component:data:treatment
  ##############################################################################

  # Derive the treatment type
  type <- initialChar(type, "pc", "'type' should be Parallel or Crossover")
  if (!missing(sequence)) {
    type <- "c"
  } else if (!missing(doses)) {
    type <- "p"
  } else {
    ectdStop("Need arguments 'sequence' or 'doses'") # : doseOrSequence
  }

  # tidy up the 'times' argument for crossover
  # and check about the sequence matrix
  if (type == "c") {
    if (missing(sequence)) { # not gonna happen {doseOrSequence}
      ectdStop("'sequence' must be supplied for a Crossover design")
    }
    if (!is.matrix(sequence) || !is.numeric(sequence)) {
      ectdStop("'sequence' must be a numeric matrix")
    }
    if (is.null(times)) times <- 1:nrow(sequence)
  }

  # tests for Parallel type
  if (type == "p" && missing(doses)) {
    ectdStop("'doses' must be supplied for parallel treatment")
  }

  doseCol <- parseCharInput(doseCol,
    convertToNumeric = FALSE,
    valid = TRUE,
    expected = 1
  )
  timeCol <- parseCharInput(timeCol,
    convertToNumeric = FALSE,
    valid = TRUE,
    expected = 1
  )
  trtCol <- parseCharInput(trtCol,
    convertToNumeric = FALSE,
    valid = TRUE,
    expected = 1
  )

  times <- parseCharInput(times)
  nTimes <- length(times)

  if (type == "p") { # then make the sequence matrix
    doses <- parseCharInput(doses)
    if (is.null(times)) {
      sequence <- matrix(doses, nrow = 1)
    } else {
      sequence <- matrix(doses,
        nrow = length(times),
        ncol = length(doses), byrow = TRUE
      )
      sequence[times < 0, ] <- 0
    }
  }

  if (type == "c") {

    # does it have the right number of rows
    if (nTimes != nrow(sequence)) {
      diffSeq <- nTimes - nrow(sequence)
      if (diffSeq > 0 && all(times[1:diffSeq] <= 0)) {
        sequence <- rbind(matrix(0,
                                 nrow = diffSeq,
                                 ncol = ncol(sequence)),
                          sequence)
      } else {
        ectdStop(
          "difference between the number of rows in the sequence matrix" %.n%
            "and the number of time points\n"
        )
      }
    }

    # no dose run-in period
    if (any(runinTimes <- times < 0) &&
        any(sequence[which(runinTimes), ] != 0)) {
      ectdStop("The sequence matrix suggests a dose run-in period")
    }
  }
  ### from this point, everything has been checked, everything should
  ### be alright to build the treatment data

  nTreat <- ncol(sequence)

  out <- .eval(
    if (type == "p" && is.null(times)) {
      "data.frame( $trtCol=1:nTreat, $doseCol=as.vector(sequence)  ) "
    } else {
      "data.frame( $trtCol=rep(1:nTreat, each=nTimes) ,
      $timeCol = rep( times, nTreat),
      $doseCol = as.vector(sequence)  ) "
    }
  )

  out
}
