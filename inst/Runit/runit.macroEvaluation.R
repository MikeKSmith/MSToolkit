if( !exists("unitTestPath")) unitTestPath <- system.file(package = "MSToolkit", "Runit")
macro.datapath <- file.path( unitTestPath , "data", "macroEvaluation" )


test.analysis.macro <- function(){
                  
  
  checkException( macroEvaluation(doseCol = "DOSE, DOSES"), 
    msg = "dose too long")
  checkException( macroEvaluation(doseCol = "01erwfwjui3w4"), 
    msg = "dose not valid")
  checkException( macroEvaluation(interimCol = "INTSA, INTERIM"), 
    msg = "interim too long")
  checkException( macroEvaluation(interimCol = "=08fewik3"), 
    msg = "interim not valid")
  
  
  microData <- read.csv( file.path(macro.datapath, "micro0001.csv" ), 
    header = TRUE )
  checkException( macroEvaluation(doseCol = "D", data = microData), 
    msg = "dose not in the data")
  checkException( macroEvaluation(interimCol = "I", data = microData), 
    msg = "interim not in the data")
  
  wrongCode <- function(data) stop("error")  
  checkException( macroEvaluation(data = microData, macroCode = wrongCode), 
    msg = "wrong code, generates error")
  
#  checkException( macroEvaluation(data = microData, macroCode = "ghost"), 
#    msg = "code does not exist")
    
  checkException( macroEvaluation(data = microData, macroCode = ghost), 
    msg = "code does not exist")
    
  mCode1 <- function(data) {
    diffMeans <- data$MEAN[ data$DOSE == 100] - 
    data$MEAN[ data$DOSE == 0   ]
    data.frame( SUCCESS = diffMeans > 10, NFINAL = sum(data$N) )
  }
  checkException( macroEvaluation(data = microData, macroCode = mCode1), 
    msg = "code generates data with more than one line")
  
  mCode2 <- function(data) {
    diffMeans <- data$MEAN[ data$DOSE == 100 & data$INTERIM == 0] - 
    data$MEAN[ data$DOSE == 0 & data$INTERIM == 0  ]
    data.frame( SUCCESS = diffMeans > 10, NFINAL = sum(data$N) )
  }
  out <- macroEvaluation(data = microData, macroCode = mCode2)
  checkTrue( all(dim( out ) == c(1,2) ) , 
    msg = "cheking with correct code" )
    
}

