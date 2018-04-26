library(lubridate)
library(tidyverse)

read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

FindRecentEvents <- function(input, intervalType, intervalLength){
  offset <- do.call(intervalType, list(intervalLength))
  interv <- interval(input - offset, input - seconds(1)) #Remove a second so it doesn't count itself.
  fx <- function(x,y) {
    count(filter(as.data.frame(y), y %within% x))
  }
  output <- as.data.frame(unlist(sapply(interv, fx, y = input)))
  colnames(output) = "NInRange"
  output
}
