test.checkMacroFormat <- function(){
  
  checkException( checkMacroFormat( 1:10 ) , 
    msg = "not a data frame")
    
  checkException( checkMacroFormat( iris[1:10, ]), 
    msg = "more than one row")  

 checkTrue(checkMacroFormat(data.frame(x =1 )))
}

