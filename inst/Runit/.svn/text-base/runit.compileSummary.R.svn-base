setEctdDataMethod("CSV")
if( !exists("unitTestPath")) unitTestPath <- system.file(package = "MSToolkit", "Runit")
compileSummary.datapath <- file.path( unitTestPath , "data", "compileSummary" )
compileSummary2.datapath <- file.path( unitTestPath , "data", "compileSummary2" )
cat("compileSummary.datapath:", compileSummary.datapath, "\n" )
cat("compileSummary2.datapath:", compileSummary.datapath, "\n" )

test.compileSummary.failure <- function(){

  checkException( compileSummary("frew"), 
    msg = "Micro or Macro" )
    
  dir.create( tf <- tempfile() )
  checkException(compileSummary("Micro", workingPath = tf ), 
     msg = "no MicroEvaluation directory" )
  checkException(compileSummary("Macro", workingPath = tf ), 
     msg = "no MacroEvaluation directory" )
  dir.create( file.path(tf, "MicroEvaluation") )
  dir.create( file.path(tf, "MacroEvaluation") )
  checkException(compileSummary("Micro", workingPath = tf ), 
     msg = "no csv files in MicroEvaluation directory" )
  checkException(compileSummary("Macro", workingPath = tf ), 
     msg = "no csv files in MacroEvaluation directory" )
  
  unlink(tf, recursive = TRUE)  
  
}

test.compileSummary.R <- function(){  
  
  ## copy files accross
  dir.create( cpdir <- file.path(tempdir(), "compileSummary.R" ) )
  dir.create( file.path(cpdir, "MacroEvaluation" ) )
  dir.create( file.path(cpdir, "MicroEvaluation" ) )
  for( i in 1:5){
    file.copy( file.path(compileSummary.datapath, "MicroEvaluation",  sprintf("micro%04d.csv", i)) , 
      file.path(cpdir, "MicroEvaluation"    ) )
    file.copy( file.path(compileSummary.datapath, "MacroEvaluation",  sprintf("macro%04d.csv", i)) , 
      file.path(cpdir, "MacroEvaluation"    ) )
  }
  
  compileSummary( "Micro", workingPath = cpdir)
  compileSummary( "Macro", workingPath = cpdir)
  expectedMicroData <- read.csv( file.path(compileSummary.datapath, "microSummary.csv") )
  newMicroData <- read.csv( file.path(compileSummary.datapath, "microSummary.csv") )
  checkEquals(expectedMicroData, newMicroData, 
    msg = "compile micro R")
  expectedMacroData <- read.csv( file.path(compileSummary.datapath, "macroSummary.csv") )
  newMacroData <- read.csv( file.path(compileSummary.datapath, "macroSummary.csv") )
  checkEquals(expectedMicroData, newMicroData, 
    msg = "compile macro R")
  
  unlink(cpdir, recursive = TRUE)
  
}

test.compileSummary.missing <- function(){  
  ## copy files accross
  dir.create( cpdir <- file.path(tempdir(), "compileSummary.missing" ) )
  dir.create( file.path(cpdir, "MacroEvaluation" ) )
  dir.create( file.path(cpdir, "MicroEvaluation" ) )
  for( i in 1:5){
    file.copy( file.path(compileSummary.datapath, "MicroEvaluation",  sprintf("micro%04d.csv", i)) , 
      file.path(cpdir, "MicroEvaluation"    ) )
    file.copy( file.path(compileSummary.datapath, "MacroEvaluation",  sprintf("macro%04d.csv", i)) , 
      file.path(cpdir, "MacroEvaluation"    ) )
  }

  checkException( compileSummary( "Micro", replicates = 1:10, workingPath = cpdir ), 
    msg = "not compile when missing files (micro)" )
  checkException( compileSummary( "Macro", replicates = 1:10, workingPath = cpdir ), 
    msg = "not compile when missing files (macro)" )

    
  
  unlink(cpdir, recursive = TRUE)
  
}


test.compileSummary.holes.R <- function(){  
  ## copy files accross
  dir.create( cpdir <- file.path(tempdir(), "compileSummary.missing" ) )
  dir.create( file.path(cpdir, "MacroEvaluation" ) )
  dir.create( file.path(cpdir, "MicroEvaluation" ) )
  for( i in 1:5){
    file.copy( file.path(compileSummary2.datapath, "MicroEvaluation",  sprintf("micro%04d.csv", i)) , 
      file.path(cpdir, "MicroEvaluation"    ) )
    file.copy( file.path(compileSummary2.datapath, "MacroEvaluation",  sprintf("macro%04d.csv", i)) , 
      file.path(cpdir, "MacroEvaluation"    ) )
  }

  checkException(compileSummary( "Micro",  workingPath = cpdir ), msg = "not all columns the same")
  checkException(compileSummary( "Macro",  workingPath = cpdir ), msg = "not all columns the same")
    
  unlink(cpdir, recursive = TRUE)
  
}
