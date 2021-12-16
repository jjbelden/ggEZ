special_paste <- function(vector){
#workaround for error with quoted parenthesis matching
  paste(vector, ")", sep="")
}
