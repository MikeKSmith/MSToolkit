
# Verbose flag
test.meta.verbose <- function(){

	currentVerbose <- getEctdVerbose ( ) 
	checkEquals(currentVerbose , get("verbose", env = .ectdEnv), "Extracts verbose flag correctly")
	checkException(setEctdVerbose(), "No input to .setEctdVerbose")
	setEctdVerbose("TEST")
	checkEquals(getEctdVerbose ( ), "TEST", "Sets the verbose flag correctly")
	setEctdVerbose(currentVerbose)
} 

# Default logging file
test.meta.logFile <- function(){

	currentLogFile <- getEctdLogFile ( ) 
	checkEquals(currentLogFile, get("logfile", env = .ectdEnv), "Extracts log file correctly")
	checkException(setEctdLogFile(), "No input to .setEctdLogFile")
	setEctdLogFile("TEST")
	checkEquals(getEctdLogFile ( ) , "TEST", "Sets the log file correctly")
	setEctdLogFile(currentLogFile)
	
} 

# Default Date Format
test.meta.dateFormat <- function(){

	currentDateFormat <- getEctdDateFormat ( ) 
	checkEquals(currentDateFormat, get("dateFormat", env = .ectdEnv), "Extracts date format correctly")
	checkException(setEctdDateFormat(), "No input to .setEctdDateFormat")
	setEctdDateFormat("TEST")
	checkEquals(getEctdDateFormat ( ) , "TEST", "Sets the date format correctly")
	setEctdDateFormat(currentDateFormat)
	
} 

# Default Data Storage Method
test.meta.dataStorageMethod <- function(){
	
	currentDataMethod <- getEctdDataMethod ( ) 
	checkEquals(currentDataMethod, get("dataStoreMethod", env = .ectdEnv), "Extracts data method correctly")
	checkException(setEctdDataMethod(), "No input to .setEctdDataMethod")
	checkException(setEctdDataMethod("X"), "Invalid method specified in .setEctdDataMethod")
	setEctdDataMethod("R")
	checkEquals(getEctdDataMethod ( ) , "RData", "Sets the data method correctly: RData")
	setEctdDataMethod("I")
	checkEquals(getEctdDataMethod ( ) , "Internal", "Sets the data method correctly: Internal")
	setEctdDataMethod("C")
	checkEquals(getEctdDataMethod ( ) , "CSV", "Sets the data method correctly: CSV")
	setEctdDataMethod(currentDataMethod)
	
} 

# Default external execution paths
test.meta.externalPath <- function(){
	
	# Calling errors: .getEctdExternalPath
	checkException(getEctdExternalPath ("XXX"), "Invalid path for .getEctdExternalPath")
	checkException(getEctdExternalPath (1), "Invalid input to .getEctdExternalPath: Numeric")
	checkException(getEctdExternalPath (letters), "Invalid input to .getEctdExternalPath: Multiple")

	# Getting a path
	currentPath <- getEctdExternalPath ( "SASPATH_UNIX" )
	checkEquals(currentPath, get("externalPaths", env = .ectdEnv)["SASPATH_UNIX"], "Extracts path correctly")

	# Calling errors: .setEctdExternalPath
	checkException(setEctdExternalPath(), "Invalid input to .setEctdExternalPath: Missing pathName")
	checkException(setEctdExternalPath(1), "Invalid input to .setEctdExternalPath: Numeric pathName")
	checkException(setEctdExternalPath(letters), "Invalid input to .setEctdExternalPath: Multiple pathName")
	checkException(setEctdExternalPath("X"), "Invalid input to .setEctdExternalPath: Missing Value")
	checkException(setEctdExternalPath("X", 1), "Invalid input to .setEctdExternalPath: Numeric Value")
	checkException(setEctdExternalPath("X", letters), "Invalid input to .setEctdExternalPath: Multiple Value")

	# Setting a path
	setEctdExternalPath("SASPATH_UNIX", "TEST")
	checkEquals(getEctdExternalPath( "SASPATH_UNIX" ) , c("SASPATH_UNIX" = "TEST"), "Sets an external path correctly")
	setEctdExternalPath("SASPATH_UNIX", currentPath)
	checkTrue(all(names(get("externalPaths", env = .ectdEnv)) == getEctdExternalPath()))
} 

# Default Column Names
test.meta.columnNames <- function(){

	# Reset all default column names
	resetEctdColNames()
	
	# Calling errors: .getEctdColName 
	checkException(getEctdColName(), "Invalid input to .getEctdColName : Missing")
	checkException(getEctdColName(1), "Invalid input to .getEctdColName : Numeric")
	checkException(getEctdColName(letters), "Invalid input to .getEctdColName : Multiple")
	checkException(getEctdColName("XXX"), "Invalid column name .getEctdColName")
	
	# Check values
	checkTrue(getEctdColName ( "Time" ) == "TIME", "Check correct Time column")
	checkTrue(getEctdColName ( "Dose" ) == "DOSE", "Check correct Dose column")
	checkTrue(getEctdColName ( "Subject" ) == "SUBJ", "Check correct Subject column")
	checkTrue(getEctdColName ( "Interim" ) == "INTERIM", "Check correct Interim column")
	checkTrue(getEctdColName ( "ParOmit" ) == "PAROMIT", "Check correct ParOmit column")
	checkTrue(getEctdColName ( "RespOmit" ) == "RESPOMIT", "Check correct RespOmit column")
	checkTrue(getEctdColName ( "Response" ) == "RESP", "Check correct Response column")
	checkTrue(getEctdColName ( "Trt" ) == "TRT", "Check correct Treatment column")
	checkTrue(getEctdColName ( "Missing" ) == "MISSING", "Check correct Missing column")
	checkTrue(getEctdColName ( "Replicate" ) == "Replicate", "Check correct Replicate column")
	
	# Calling errors: .setEctdColName
	checkException(setEctdColName(), "Invalid input to .setEctdColName: Missing colName")
	checkException(setEctdColName(1), "Invalid input to .setEctdColName: Numeric colName")
	checkException(setEctdColName(letters), "Invalid input to .setEctdColName: Multiple colName")
	checkException(setEctdColName("X"), "Invalid input to .setEctdColName: Missing Value")
	checkException(setEctdColName("X", 1), "Invalid input to .setEctdColName: Numeric Value")
	checkException(setEctdColName("X", letters), "Invalid input to .setEctdColName: Multiple Value")
	checkException(setEctdColName("X", "TEST"), "Invalid input to .setEctdColName: Non-matching Column Name")

	# Setting a default column name
	setEctdColName("Dose", "TEST")
	checkEquals(getEctdColName( "Dose" ) , "TEST", "Sets a default column name correctly")
	
	# Reset all default column names
	resetEctdColNames()
	
	# Check values
	checkTrue(getEctdColName ( "Time" ) == "TIME", "Check correct Time column")
	checkTrue(getEctdColName ( "Dose" ) == "DOSE", "Check correct Dose column")
	checkTrue(getEctdColName ( "Subject" ) == "SUBJ", "Check correct Subject column")
	checkTrue(getEctdColName ( "Interim" ) == "INTERIM", "Check correct Interim column")
	checkTrue(getEctdColName ( "ParOmit" ) == "PAROMIT", "Check correct ParOmit column")
	checkTrue(getEctdColName ( "RespOmit" ) == "RESPOMIT", "Check correct RespOmit column")
	checkTrue(getEctdColName ( "Response" ) == "RESP", "Check correct Response column")
	checkTrue(getEctdColName ( "Trt" ) == "TRT", "Check correct Treatment column")
	checkTrue(getEctdColName ( "Missing" ) == "MISSING", "Check correct Missing column")
	checkTrue(getEctdColName ( "Replicate" ) == "Replicate", "Check correct Replicate column")

	# Check defaults
	checkEquals(getEctdColName ( "Time" ), get("colNames", env = .ectdEnv)$Time$Default, "Check default Time column")
	checkEquals(getEctdColName ( "Dose" ), get("colNames", env = .ectdEnv)$Dose$Default, "Check default Dose column")
	checkEquals(getEctdColName ( "Subject" ), get("colNames", env = .ectdEnv)$Subject$Default, "Check default Subject column")
	checkEquals(getEctdColName ( "Interim" ), get("colNames", env = .ectdEnv)$Interim$Default, "Check default Interim column")
	checkEquals(getEctdColName ( "ParOmit" ), get("colNames", env = .ectdEnv)$ParOmit$Default, "Check default ParOmit column")
	checkEquals(getEctdColName ( "RespOmit" ), get("colNames", env = .ectdEnv)$RespOmit$Default, "Check default RespOmit column")
	checkEquals(getEctdColName ( "Response" ), get("colNames", env = .ectdEnv)$Response$Default, "Check default Response column")
	checkEquals(getEctdColName ( "Trt" ), get("colNames", env = .ectdEnv)$Trt$Default, "Check default Trt column")
	checkEquals(getEctdColName ( "Missing" ), get("colNames", env = .ectdEnv)$Missing$Default, "Check default Missing column")
	checkEquals(getEctdColName ( "Replicate" ), get("colNames", env = .ectdEnv)$Replicate$Default, "Check default Replicate column")

	# Calling errors: .getEctdPossibleColNames
	checkException(getEctdPossibleColNames(), "Invalid input to .getEctdPossibleColNames: Missing")
	checkException(getEctdPossibleColNames(1), "Invalid input to .getEctdPossibleColNames: Numeric")
	checkException(getEctdPossibleColNames(letters), "Invalid input to .getEctdPossibleColNames: Multiple")
	checkException(getEctdPossibleColNames("XXX"), "Invalid column name .getEctdPossibleColNames")

	# Possible Columns
	checkTrue(getEctdColName ( "Time" ) %in% getEctdPossibleColNames("Time"), "Initial contained in possible Time values")
	checkTrue(getEctdColName ( "Dose" ) %in% getEctdPossibleColNames("Dose"), "Initial contained in possible Dose values")
	checkTrue(getEctdColName ( "Subject" ) %in% getEctdPossibleColNames("Subject"), "Initial contained in possible Subject values")
	checkTrue(getEctdColName ( "Interim" ) %in% getEctdPossibleColNames("Interim"), "Initial contained in possible Interim values")
	checkTrue(getEctdColName ( "ParOmit" ) %in% getEctdPossibleColNames("ParOmit"), "Initial contained in possible ParOmit values")
	checkTrue(getEctdColName ( "RespOmit" ) %in% getEctdPossibleColNames("RespOmit"), "Initial contained in possible RespOmit values")
	checkTrue(getEctdColName ( "Response" ) %in% getEctdPossibleColNames("Response"), "Initial contained in possible Response values")
	checkTrue(getEctdColName ( "Trt" ) %in% getEctdPossibleColNames("Trt"), "Initial contained in possible Treatment values")
	checkTrue(getEctdColName ( "Missing" ) %in% getEctdPossibleColNames("Missing"), "Initial contained in possible Missing values")
	checkTrue(getEctdColName ( "Replicate" ) %in% getEctdPossibleColNames("Replicate"), "Initial contained in possible Replicate values")
	checkTrue(all("ID" %in% getEctdPossibleColNames("Subject")), "Extra possible Subject values")
	checkTrue(all(c("DAY", "WEEK") %in% getEctdPossibleColNames("Time")), "Extra possible Time values")
	checkTrue(all("DV" %in% getEctdPossibleColNames("Response")), "Extra possible Response values")
	checkTrue(all("TRIAL" %in% getEctdPossibleColNames("Replicate")), "Extra possible Replicate values")
	
	# Setting and getting possible column names
	checkException(getEctdPossibleColNames("X"))
	checkException(getEctdPossibleColNames(c("Subject", "Dose")))
	checkException(setEctdPossibleColNames("X", LETTERS))
	checkException(setEctdPossibleColNames(c("Subject", "Dose"), LETTERS))
	checkException(setEctdPossibleColNames("Subject", 1:10))
	resetEctdColNames()
	origPoss <- c("SUBJ", "ID")
	checkTrue(all(getEctdPossibleColNames("Subject") == origPoss))
	setEctdPossibleColNames("Subject", LETTERS)
	checkTrue(all(getEctdPossibleColNames("Subject") == c("SUBJ", LETTERS)))
	setEctdPossibleColNames("Subject", origPoss)
	checkTrue(all(getEctdPossibleColNames("Subject") == origPoss))

	# Matching the possible column names
	resetEctdColNames()
	setEctdPossibleColNames("Subject", origPoss)
	checkTrue(!length(matchEctdColNames("Subject", LETTERS)))
	checkTrue(matchEctdColNames("Subject", c(letters, "SUBJ")) == "SUBJ")
	
} 

