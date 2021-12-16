![]tanager-g04378c85c_640.jpg

# ggEZ


The **ggEZ** provides a function for easily getting new code to solve elementary problems 
in ggplot, such as adding color and labels, and better visualizing hard to see data.

## Installation

You can install the development version of qacReg using:

``` r
if(!require(remotes)){
  install.packages("remotes")
}
remotes::install_github("jjbelden/ggEZ")
```

## Example

Here is a basic example:

``` r
library(ggEZ)
newPlot <- ggEZ("ggplot(data = mtcars, aes(x = hp, y = mpg)) +
  geom_point(  )", problem = "color")
newerPlot <- ggEZ(newPlot, problem = "clustered_data")  
print(newPlot)
print(newerPlot)
```
