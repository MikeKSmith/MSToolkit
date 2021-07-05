#' Missing Completely at Random
#' 
#' This function adds (or modifies) a "MISSING" flag to a dataset to simulate a
#' missing completely at random behaviour.
#' 
#' The missing data is either added to the dataset or modified if it already
#' exist. In the latter case, the function only overwrites data that is not
#' already missing.
#' 
#' @param data (Required) Data frame to which to add missingness
#' @param prop (Optional) proportion of missingness between 0 and 1.  The
#' default is "0" (so no missingness is generated)
#' @param rule (Optional) Only observations matching the rule can be flagged as
#' missing.  Be default, all observations are available to be missing
#' @param seed (Optional) Random seed to use.  Based on the current random seed
#' by default
#' @param flagName (Optional) name of the missing flag ("MISSING" by default)
#' @return the \code{data} argument to which a MISSING flag is added or
#' modified.
#' @author Mike K Smith \email{mstoolkit@@googlemail.com}
#' @seealso \code{\link{createDropout}} for drop out missingness.
#' 
#' \code{\link{parseRangeCode}} to handle the \code{rule} argument.
#' @keywords datagen
#' @examples
#' 
#'   
#' myData <- data.frame( 
#'   SUBJ   = rep(1:3, each = 3), 
#'   TIME = rep(0:2, 3)  ) 
#' createMCAR( myData, prop = 0.1, rule = "TIME > 0")  
#' 
#' \dontrun{
#'  ## more examples in the unit tests
#'  file.show( system.file( "Runit", "runit.data.missing.R" , package = "MSToolkit") ) 
#' }
#' 
#' 
"createMCAR" <- function(
	data,                             #@ Data Structure to which to add missing flag
	prop = 0,                         #@ Proportion of observations to set to missing
	rule,                             #@ Rule to specify which observations of the data shold be included
	seed = .deriveFromMasterSeed( ),  #@ Random seed
	flagName = getEctdColName("Missing")
){
	###############################################################################
	# Mango Solutions, Chippenham SN14 0SQ 2006
	# createMCAR.R Fri Jun 08 08:56:54 BST 2007 @372 /Internet Time/
	#
	# Author: Romain    
	###############################################################################
	# DESCRIPTION: missing completely at random flags
	# KEYWORDS: component:data:missingness
	###############################################################################

	set.seed( seed )
	validNames( flagName )
  
	## handle correct values of `prop`
	if( prop < 0 || prop > 1 ) ectdStop( "`prop` must be within range [0,100]" )  
	if( prop == 0 ){
		if( !(flagName %in% colnames(data)) ) data[[flagName]] <- rep(0, nrow(data))
		return( data ) # don't have to care about the rule in that case
  }
  
	## check if the existing missing flag
	if( flagName %in% colnames(data) && !all(data[[flagName]] %in% 0:1)  ) {
		ectdStop( "The missing flag ($flagName) does not only contain 0 and 1" )
	}
  
	nr <- nrow(data)
  
	if( missing(rule)){ 
		include <- rep(TRUE, nr )    
	} 
	else {
    	rule <- parseRCode( rule )
    	include <- try( eval( rule, data ) , silent = TRUE)
    	## check if something went wrong when executing the code
    	if( class(include) == "try-error" ) {
    		ectdStop( "The rule is not correct R code : " %.nt% ( include %-~% "^[^:]*:"  ) )
		}
    
		## checks on the output of the rule
    	if( !is.logical(include) ) ectdStop("The `rule` does not produce a logical vector") 
		if( length(include) != nrow(data) ){
    		ectdStop(
        		"Dimension problem with `rule` code" %.n% 
        		"\trows in the data: $nr, length of logical vector: " %.% length(include)  ) 
		}
    
		if( ! any( include ) ) ectdStop( "All logical values are false"  )
	} 
  
	### sample the missing values
	missingSample <- sample( c(1, 0), prob = c( prop, 1 - prop), size = sum(include), replace = TRUE )
  
	### create the new missing flag  
  	missingFlag <- replace( rep(0, nrow(data)), include, missingSample )
    
	### add or modify the missing flag in the data set  
	data[[ flagName ]] <- if( flagName %in% colnames(data) ){
		1 * ( data[[ flagName ]] | missingFlag ) 
	} else missingFlag
  data 
  
}
