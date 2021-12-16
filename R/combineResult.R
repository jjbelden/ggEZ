#' combineResult
#'
#' @param list a list
#'
#' @return a string composed of the elements of the list concatenated
combineResult <- function(list){
  result <-  ""
  initial <- TRUE

  for (i in list){

    if (initial == FALSE){
      result <- paste(result, i, sep = " + ")
    }

    else if (initial == TRUE){
      result <- paste(result, i, sep = "")
      initial <- FALSE
    }
  }
  return (result)
}

