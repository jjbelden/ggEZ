#' quotecheck
#'
#' @param str a string
#'
#' @return a vector composed of close parentheses equal to the number of
#' open parentheses in the string
#'
#'
quotecheck <- function(str){
  library(stringr)
  vector <- ""
  strCount <- str_count(str, '\\(')

  for(i in 1:strCount){
    vector <- special_paste(vector)

  }
 return (vector)
}

