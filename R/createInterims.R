#' Create the interim flag
#'
#' In clinical trials there is sometimes the need to examine emerging data as
#' the trial progresses to make decisions about curtailing the trial early,
#' either due to lack of efficacy or because statistical significance is
#' achieved and it would be unethical to continue allocating patients to
#' inferior treatments. These interim assessments are usually performed after
#' a specified proportion of subjects have completed treatment. Thus, there is
#' a cumulative proportion of patients eligible at each interim assessment.
#' MSToolkit allows the user to specify this cumulative proportion and then
#' allocates subjects to each interim assessment.
#'
#' This function creates the \emph{INTERIM} flag that identifies which interim
#' analysis is the \emph{FIRST} for each subject
#'
#' If the "method" argument is set to "Sample", the interim numbers should be
#' assigned using the following algorithm: Store the number of subjects
#' (nSubjects) and the number of interims (nInterims) Append a zero to the
#' start of the proportions and take differences to get a vector of
#' "non-cumulative" probabilities Take "nSubjects" samples from the values "1"
#' to "nInterims" with probabilities set to the derived "non-cumulative"
#' probabilities Replace the "interim" variable with these values.
#'
#' If the "method" argument is set to "Proportion", the interim numbers should
#' be assigned using the following algorithm: Store the number of subjects
#' (nSubjects) and the number of interims (nInterims) Convert the cumulative
#' proportions to "non-cumulative" probabilities. Allocate a "base" number of
#' subjects to each interim (found by multiplying nSubjects by the set of
#' probabilities, then take the floor of the resulting values) If any subjects
#' are left at this stage, use the "sample" method (below) to allocate the
#' remaining subjects to interims "Shuffle" the interim variable so that the
#' results are not ordered.
#'
#' @param subjects (Required) Vector of subjects or number of subjects
#' @param proportion (Required) Vector of proportions of subjects in each
#' interim.  This is a vector of cumulative proportions
#' @param seed (Optional) The random number generation seed to use.  By
#' default, this is derived from the current random seed
#' @param idCol (Optional) Name of the subject column ("SUBJ" by default)
#' @param interimCol (Optional) Name of the interim column ("INTERIM" by
#' default)
#' @param method (Optional) Method to use. Must be one of \emph{Sample} or
#' \emph{Proportion}.  Default is \emph{Sample}
#' @return A data frame containing the following variables: \item{SUBJ}{Subject
#' identifier, named after \code{idCol}.} \item{INTERIM }{Interim flag, named
#' after \code{interimCol}.}
#' @author Romain Francois
# @seealso \code{\link{generateData}}
#' @keywords datagen
#' @examples
#'
#'   # first interim with 10% of the subjects
#'   # second with 30%
#'   # third with 60%
#'   interims <- createInterims(100, proportion  = ".1,.3,.6" )
#'   prop.table(table(interims$INTERIM))
#'
#'   \dontrun{
#'     ## more examples in the unit tests
#'     file.show( system.file("Runit", "runit.data.interim.R",
#'                 package = "MSToolkit") )
#'   }
#'
#' @export
createInterims <- function(
  subjects,
  proportion,
  seed = .deriveFromMasterSeed(),
  idCol = getEctdColName("Subject"),
  interimCol = getEctdColName("Interim"),
  method = "Sample"
  ){
  ###############################################################################
  # Mango Solutions, Chippenham SN14 0SQ 2006
  # createContinuousCovariates.R Fri Jun 01 10:41:35 BST 2007 @445 /Internet Time/
  #
  # Author: Romain Francois
  ###############################################################################
  # DESCRIPTION: create the interim data
  # KEYWORDS: datagen, component:data:interim
  ###############################################################################

  set.seed(seed)

  ## validate the names
  validNames( idCol, interimCol)
  if( idCol == interimCol ){
    ectdStop("`idCol` and `interimCol` should be different")
  }

  ## tidy up the method argument
  method <- initialChar(method, "ps", "method must be `Sample` or `Proportion`")

  ## handle the case where sujects is of length 1
  subjects <- .expandSubjects( subjects )
  nSubjects <- get("nSubjects")

  ## generate the non-cumulative proportions from the cumulative
  proportion <- if( missing(proportion) || is.null(proportion) ) {
    1
  } else {
    .nonCumulativeFromCumulative( proportion )
  }
  interimValues <- seq( along = proportion)

  ## generate the interim code
  interim <- switch(method,
    "s" = {    # Sample method
      sample(interimValues,
             size = nSubjects,
             prob = proportion,
             replace = TRUE)
    },
    "p" = {    # Proportion method
       sizes <- floor( nSubjects * proportion )
       interim <- c( rep( interimValues, sizes ),
         sample(interimValues,
                prob = proportion,
                size = nSubjects - sum(sizes) ) )
       interim <- sample( interim )
     })

  .eval( "data.frame( $idCol = subjects, $interimCol = interim)" )

}

