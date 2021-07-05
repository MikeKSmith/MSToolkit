#' Allocate treatment to subjects
#'
#' The \emph{Allocate} component is responsible for allocating simulated
#' subjects to a treatment group.
#'
#' Subjects numbered "1" to "sum(subjects)" are allocated to a single
#' treatment or treatment sequence.  The possible treatments are numbered "1"
#' to "trts".
#'
#' If "subjects" is a single number, the proportion argument is used to
#' determine the proportion to be allocated to each treatment.  By default,
#' subjects are allocated randomly to each treatment group with probability
#' "1/trts".
#'
#' If "subjects" is a vector with length "trts", this explicity defines the
#' number of subjects to be allocate to each treatment group.
#'
#' If "ordered" is TRUE, the allocation is done in order (eg. subject 1 gets
#' treatment 1). This can be useful in simulations to quickly verify that the
#' correct number or proportion of subjects is allocated to each treatment. If
#' "FALSE", random allocation is performed.
#'
#' @param trts (Required) Maximum number of treatments to which subjects can be
#' allocated.  Subjects will be allocated to treatments "1:trts"
#' @param subjects (Required) Number of subjects to be allocated to each
#' treatment group
#' @param prop (Optional) Proportion of subjects in each group.  By default,
#' equal proportions of subjects are assigned to each treatment arm
#' @param ordered (Optional) Should treatments be allocated in order of subject
#' number (ie. first N subjects gets treatment 1) as opposed to random
#' assignment.  Default is FALSE (random assignment)
#' @param seed (Optional) Random seed to allocate interims.  Based on the
#' current master seed by default
#' @param idCol (Optional) Subject variable name.  "SUBJ" by default
#' @param trtCol (Optional) Treatment variable name.  "TRT" by default
#' @return A data frame with subjects and treatment allocations.  This data
#' frame will contain 2 variables: \code{SUBJ}The Subjects identifier
#' \code{TRT}The Treatment numeric that the subject is allocated to ...
#' @author Rich Pugh
# @seealso \code{\link{createTreatments}} to create the set of possible
# treatments given a sequence matrix for a crossover design or a set of doses
# for a parallel design.
#'
# \code{\link{generateData}} that wraps this function.
#' @keywords datagen
#' @examples
#'
#'   # allocate 6 subjects randomly to 3 treatment groups
#'   allocateTreatments(trts = 3, subjects = 6)
#'
#'   # allocate 6 subjects randomly to 3 treatment groups
#'   #  and present in treatment order
#'   allocateTreatments(trts = 3, subjects = 6, ordered = TRUE)
#'
#'   # allocate 2 subjects to group 1, 2 to group 2, 3 to group 3
#'   # First two subjects will be allocated to TRT 1
#'   allocateTreatments(trts = 3, subjects = c(2, 2, 3), ordered = TRUE)
#'
#'   # allocation according to proportions
#'   # 6 subjects to allocate in total in 2 groups
#'   # 20% will be in group 1, 80% will be in group 2
#'   allocateTreatments(trts = 2, subjects = 6, prop = c(0.2, 0.8))
#'
#'   # allocation according to proportions
#'   # 6 subjects to allocate in total in 2 groups
#'   # 20% will be in group 1, 80% will be in group 2
#'   # TRT 1 will be allocated first
#'   allocateTreatments(trts = 2,
#'                      subjects = 6,
#'                      prop = c(0.2, 0.8),
#'                      ordered = TRUE)
#'
"allocateTreatments" <- function(
 	trts,
 	subjects,
 	prop = NULL,
 	ordered = FALSE,
 	seed = .deriveFromMasterSeed( ),
 	idCol = getEctdColName("Subject"),
	trtCol = getEctdColName("Trt")
) {
	##############################################################################
	# Mango Solutions, Chippenham SN15 1BN 2009
	# allocateTreatments.R 17/12/09
	#
	# Author: Rich Pugh
	##############################################################################
	# DESCRIPTION: allocate treatments to subjects
	# KEYWORDS: datagen, component:data:allocate
	##############################################################################

	# Set the seed
	set.seed(seed)

	# Check inputs
	validNames( idCol, trtCol )
	if(idCol == trtCol) ectdStop("`idCol` and `trtCol` should be different")
	subjects <- parseCharInput( subjects )
  	trts     <- parseCharInput( trts )
	prop     <- parseCharInput( prop )

	# Create treatment vector
	if (length(trts) > 1) trts <- sort(unique(trts)) else trts <- 1:trts
	nTrts <- length(trts)

	# Check (or build) proportions
	if( is.null(prop) ) prop <- rep( 1/nTrts, nTrts)
	if( sum(prop) != 1 ) ectdStop( "`prop` does not sum up to one")
	if( length(prop) != nTrts) ectdStop(
	  "`prop` should have the same length as the number of treatments: $trts"
	  )

	if( any(subjects < 0)) ectdStop( "Negative value in `subjects`")
	if( length(subjects) != 1 && length( subjects ) != nTrts) {
		ectdStop(
		  "When providing a vector of `subjects`, it must be the same length as the
		  number of treatments: $trts"
		  )
	}

	# Perform the allocation
	nSubjects <- sum( subjects )
	alloc <- if( length(subjects) == 1 ) sample( trts,
	                                             replace=TRUE,
	                                             size = subjects,
	                                             prob = prop)
	else rep( trts, subjects )

	if( !all(trts %in% unique(alloc))) ectdWarning(
	  "Not all the treatments have been allocated"
	  )

	# Deal with ordered vs random
	if( ordered && is.unsorted(alloc) ) alloc <- sort( alloc )
	if( !ordered ) alloc <- sample( alloc )

	# Create output data frame
	outDf <- data.frame(1:sum(subjects), alloc)
	names(outDf) <- c(idCol, trtCol)
	outDf

}

