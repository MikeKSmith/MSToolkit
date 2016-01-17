
setEctdDataMethod("CSV")

if( !exists("unitTestPath")) unitTestPath <- system.file(package = "MSToolkit", "Runit")
analyseRep.datapath <- file.path( unitTestPath , "data", "analyseRep" )
cat("analyseRep.datapath:", analyseRep.datapath, "\n" )

test.analyseRep <- function(){
  
  checkException( analyzeRep( replicate = 2:7  )          , 
    msg = "check the handling of the replicate arg, must be of length one")
  
  checkException( analyzeRep( replicate = -3  )           , 
    msg = "check the handling of the replicate arg, must be positive")
  
  checkException( analyzeRep( replicate = "notanumber"  ) , 
    msg = "check the handling of the replicate arg, must be a number")
  
  wrongfun <- function( notdata ){
    notdata
  }
  checkException( analyzeRep( replicate = 1, analysisCode = wrongfun  ) , 
    msg = "analysis code must have a data argument")
  checkException( analyzeRep( replicate = 1, analysisCode = "doesnotexists"  ) , 
    msg = "analysis code must be available")
  
  dummyAnalysisCode <- function(data) data
  checkException( analyzeRep( replicate = 1, analysisCode = dummyAnalysisCode, 
     doseCol = "DOSE, DOSES"  ) , 
     msg = "check the dose, must be of length one" )  
  
  checkException( analyzeRep( replicate = 1, analysisCode = dummyAnalysisCode, 
     doseCol = "-97DOSE"  ) , 
     msg = "check the dose, must be valid name" )  
  
  checkException( analyzeRep( replicate = 2, analysisCode = dummyAnalysisCode,
    workingPath = analyseRep.datapath ), 
     msg = "wrong sort of input file, not same number of fields" )

  checkException( analyzeRep( replicate = 3, analysisCode = dummyAnalysisCode,
     workingPath = analyseRep.datapath ) , 
     msg = "wrong sort of input file, does not have a DOSE column" )
  
  # paromit   
  checkException( analyzeRep( replicate = 1, analysisCode = dummyAnalysisCode,
     workingPath = analyseRep.datapath, 
     parOmitFlag = "PAROMIT,PARMOIT2" ), 
     msg = "wrong sort of paromit flag, too long" )
  
  checkException( analyzeRep( replicate = 1, analysisCode = dummyAnalysisCode,
     workingPath = analyseRep.datapath, 
     parOmitFlag = "@954fgPAROMIT" ) , 
     msg = "wrong sort of paromit flag, wrong name" )
          
  # respomit flag
  checkException( analyzeRep( replicate = 1, analysisCode = dummyAnalysisCode,
     workingPath = analyseRep.datapath, 
     respOmitFlag = "RESPOMIT,RESPMOIT2" ), 
     msg = "wrong sort of respomit flag, too long" )
  
  checkException( analyzeRep( replicate = 1, analysisCode = dummyAnalysisCode,
     workingPath = analyseRep.datapath, 
     respOmitFlag = "@954fgRESPOMIT" ), 
     msg = "wrong sort of respomit flag, wrong name" )
          
  # missomit flag
  checkException( analyzeRep( replicate = 1, analysisCode = dummyAnalysisCode,
     workingPath = analyseRep.datapath, 
     missingFlag = "MISSING,MISSING2" ), 
     msg = "wrong sort of missomit flag, too long" )
  
  checkException( analyzeRep( replicate = 1, analysisCode = dummyAnalysisCode,
     workingPath = analyseRep.datapath, 
     missingFlag = "@954fgMISSING" ), 
     msg = "wrong sort of missomit flag, wrong name" )
          
  # interim
  checkException( analyzeRep( replicate = 1, analysisCode = dummyAnalysisCode,
     workingPath = analyseRep.datapath, 
     interimCol = "INTERIM,INTERIM2" ), 
     msg = "wrong sort of interim flag, too long" )
  
  checkException( analyzeRep( replicate = 1, analysisCode = dummyAnalysisCode,
     workingPath = analyseRep.datapath, 
     interimCol = "@954fginterim" ), 
     msg = "wrong sort of imterim flag, wrong name" )
     
  interimCode <- function( data ){
    outList <- list()
    outList$STOP <- FALSE
    outList
  }

  anaCode <- function(data){
   with( data, {
     uniDoses <- sort( unique(DOSE))
     outDf <- data.frame( DOSE = uniDoses, 
       MEAN = tapply(RESP, DOSE, mean) , 
       SE   = tapply(RESP, DOSE, sd )  )
     outDf$LOWER <- outDf$MEAN - 2 * outDf$SE
     outDf$UPPER <- outDf$MEAN + 2 * outDf$SE
     outDf$N     <- table(DOSE)[ as.character(uniDoses) ]
     outDf 
   }) 
  }
  res <- analyzeRep( replicate = 1, analysisCode = anaCode, interimCode = interimCode,
    workingPath = analyseRep.datapath )
  checkTrue( all( c("INTERIM","INTERIMC","DOSE","MEAN","SE","LOWER","UPPER","N","DROPPED","STOPPED")   %in% names(res) ), 
    msg = "check that the correct names are in the result" )

  idata <- read.csv(file.path( analyseRep.datapath, "ReplicateData", "replicate0001.csv" ), header = TRUE, 
    row.names = NULL )  
  checkEquals( c(0,sort(unique(idata$INTERIM))), sort(unique(res$INTERIM)), 
    msg = "checking the interim column")
  checkTrue( all ( res$INTERIMC[ res$INTERIM == 0] == "FULL" )  , 
    msg = "checking the INTERIMC column, full")
  checkTrue( all ( res$INTERIMC[ res$INTERIM == max( res$INTERIM) ] == "FINAL" )  , 
    msg = "checking the INTERIMC column, final")
    
  subFin  <- subset( res , INTERIMC == "FINAL" )  
  subFull <- subset( res , INTERIMC == "FINAL" )  
  rownames(subFin) <- rownames(subFull) # to make identical happy
  checkEquals( subFin, subFull, 
    msg = "checking final and full analysis, must be similar when no changes" 
  )
  
 n <- with( subset(res, INTERIMC != "FULL" ), 
   do.call( cbind, tapply(N, INTERIM, function(x) x ))   )
 checkTrue(  all( apply( n, 1, diff ) >= 0 ), 
   msg = "checking that the number of subjects grows as the interim grows" )

 checkTrue( all( res$LOWER <= res$UPPER ), 
    msg = "check that lower is lower than upper" ) 
  
  
}
