#' ggEZ
#' @description
#' Provides recommendation graph and code for common
#' ggplot hurdles
#'
#' @details
#' Takes a users functioning ggplot call statement and a problem they
#' are dealing with and provides potential solutions. If they select
#' \code{problem = "labels} a new ggplot graph and call statement are
#' provided with a \code{labs()} argument. If \code{problem = "color"}
#' a new ggplot graph and call statement are provided with a color and
#' fill color added, as well as a plot background color. Finally,
#' if \code{problem = "clustered_data"} is chosen three graph and call
#' statements are printed with three possible solutions to the problem,
#' one is the \code{size = x} function, a second is the \code{alpha = x}
#' function, to change transparency, and the final is the
#' \code{position_jitter()} function, which adds random noise to the data
#' for when data points overlap.
#'
#' @param ggcall a string which is also a working ggplot call statement.
#'  Must be marked with single quotes. No other modification from original call
#'  statement needed.
#'
#' @param problem the problem with your current graph which you are
#' looking for help with. Options include: \code{problem = "labels},
#' \code{problem = "color"}, and \code{problem = "clustered_data"}.
#'
#' @return
#' returns the a string representing a new ggplot call based on the original
#' ggcall given by user. This string is a usable ggplot call statement as soon
#' as the backslashes and exterior single quotes are removed.
#' @export
#'
#' @examples
#'new.graph <- ggEZ('ggplot(mtcars, aes(x=drat  )) + geom_density()', problem = "labels")
#'new.graph2 <- ggEZ(new.graph, problem = "color")

ggEZ <- function(ggcall, problem){

  library(ggplot2)

  count <- 0

  gglist <- strsplit(ggcall, split = "+", fixed=TRUE)
  #divides the ggplot call string into elements, removing and splitting by +
  stopwords <- c("\n", "))", ")" )
  #Phrases removed so that it will work as a function
  #and so we can paste() onto the end of strings
  gglist <- removeWords(gglist, stopwords)
  #removeWords function gives us a new list with above characters removed

  result.list <- vector(mode = "list", length = length(gglist[[1]]))
  #empty vector with length == gglist

  if (problem == "labels"){

      for (i in gglist){
        count <- count+1
        if (grepl("labs", i)){ warning("Your code already has a labs() function")}
        x <- paste(i, quotecheck(i))
        result.list[count] <- x

      result.list[count+1] <-
        'labs(x="X Axis", y="Y Axis ", title="Title")'
      result.list[count+2] <-
        'theme(plot.title = element_text(size=30))'
      }
    print("The labs function give you control over labels and titles. No need to stick with what R gives you")
    }

  else if (problem == "color"){

    for (i in gglist){
      count <- count+1
      if (grepl("color", i)){ warning("Your code already has color in it")}
      if (grepl("geom", i)){

        check <- strsplit(i, "\\(")
        #used only in the following if statement
        if ( length ( check[[1]] ) == 1|
             grepl ( "^\\s*$", try( check[[1]][[2]] , silent = TRUE ))){
        #checks if the geom statement is empty, if so no coma is needed
          x <- paste(i, 'color = "red"')
          x <- paste(x, ', fill = "lavender"', quotecheck(i))
          #This will only work if your geom is 2D -
          #i.e. it has no effect for geom_point()
        }

        else{

            x <- paste(i, ', color = "red"')
            x <- paste(x, ', fill = "lavender"', quotecheck(i))
        }

        result.list[count] <- x
      }
      else{
        x <- paste(i, quotecheck(i))
        result.list[count] <- x
      }
    }
    result.list[count+1] <-
      'theme(plot.background = element_rect(fill = "pink"))'
    print("Now you can see your original graph with some color added")
  }

   else if (problem == "colour"){
  #identical to color
    for (i in gglist){
      count <- count+1
      if (grepl("colour", i)){ warning("Your code already has colour in it")}

      if (grepl("geom", i)){
        check <- strsplit(i, "\\(")

        if ( length ( check[[1]] ) == 1|
             grepl ( "^\\s*$", try( check[[1]][[2]] , silent = TRUE ))){
          x <- paste(i, 'colour = "red"')
          x <- paste(x, ', fill = "lavender"', quotecheck(i))
          #This will only work if your geom is 2D -
          #i.e. it has no effect for geom_point()
        }

        else{

          x <- paste(i, ', colour = "red"')
          x <- paste(x, ', fill = "lavender"', quotecheck(i))

        }
        result.list[count] <- x
      }
      else{
        x <- paste(i, quotecheck(i))
        result.list[count] <- x
      }
    }
    result.list[count+1] <-
      'theme(plot.background = element_rect(fill = "pink"))'
    print("Now you can see your original graph with some colour added")
  }

  else if (problem == "clustered_data"){

    for (i in gglist){
      count <- count+1
      if (grepl("size", i)){break}
      if (grepl("geom", i)){

        check <- strsplit(i, "\\(")
        if ( length ( check[[1]] ) == 1|
             grepl ( "^\\s*$", try( check[[1]][[2]] , silent = TRUE ))){

          x <- paste(i, "size = 0.5", quotecheck(i))
        }

        else{

          x <- paste(i, ", size = 0.5", quotecheck(i))
        }

        result.list[count] <- x
      }

      else{
        x <- paste(i, quotecheck(i))
        result.list[count] <- x
      }
    }

    count <- 0
    result.str <- combineResult(result.list)
    print(result.str)
    print(eval(parse(text = result.str)))
    print("Here is your graph with all points at half their original size. Does that make it easier to see what's happening?")
    result.list <- vector(mode = "list", length = length(gglist[[1]]))

    for (i in gglist){
      count <- count+1
      if (grepl("alpha", i)){break}
      if (grepl("geom", i)){

        check <- strsplit(i, "\\(")
        if ( length ( check[[1]] ) == 1|
             grepl ( "^\\s*$", try( check[[1]][[2]] , silent = TRUE ))){

          x <- paste(i, "alpha = 0.25", quotecheck(i))
        }

        else{
          x <- paste(i, ", alpha = 0.25", quotecheck(i))
        }
        result.list[count] <- x
      }

      else{
        x <- paste(i, quotecheck(i))
        result.list[count] <- x
      }
    }
    count <- 0
    result.str <- combineResult(result.list)
    print(result.str)
    print(eval(parse(text = result.str)))
    print("Here is your graph with increased transparency. Easier on the eyes?")
    result.list <- vector(mode = "list", length = length(gglist[[1]]))

    for (i in gglist){
      count <- count+1
      if (grepl("jitter", i)){break}
      if (grepl("geom", i)){

        check <- strsplit(i, "\\(")
        if ( length ( check[[1]] ) == 1|
             grepl ( "^\\s*$", try( check[[1]][[2]] , silent = TRUE ))){

          x <- paste(i, "position = position_jitter(width = 0.4, height = 0.4)"
                     , quotecheck(i))
        }

        else{
          x <- paste(i, ", position = position_jitter(width = 0.4, height = 0.4)"
                     , quotecheck(i))
        }
        result.list[count] <- x
      }
      else{
        x <- paste(i, quotecheck(i))
        result.list[count] <- x
      }
    }
    print("This function gives every point some random noise. Here, 40% of the resolution of the data. This should be helpful if many of your points share a value.")
  }

  result.str <- combineResult(result.list)
  print(eval(parse(text = result.str)))
  return(result.str)
}

