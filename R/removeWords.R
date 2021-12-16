#' removeWords
#'
#' @param list a list
#' @param stopwords a vector of strings to be removed from
#' each element of the list
#'
#' @return a modification of the original list without any of the specified
#' strings
#'
#' @details credit to user Mikko on stack overflow
#' for the original remove function which I modified
#'
#'
#' @examples
removeWords <- function(list, stopwords) {
  count <- 0
  len <- length(list[[1]])
  result <- vector(mode = "list", length = len)
  for (i in list[[1]]){
    count <- count + 1
    x <- unlist(strsplit(i, " "))
    x <- unlist(strsplit(x, ")"))
    y <- paste(x[!x %in% stopwords], collapse = " ")
    result[count] <- y

  }
  return(result)
}



