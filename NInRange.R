read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

FindRecentEvents <- function(input, intervalType, intervalLength){
  #theSum <- sum(x)
  offset <- do.call(intervalType, list(intervalLength))
  interv <- interval(input - offset, input - seconds(1)) #Remove a second so it doesn't count itself.
  #apply(x, 1, function(y) y/sum(x))
  fx <- function(x,y) {
    count(filter(as.data.frame(y), y %within% x))
  }
  output <- as.data.frame(unlist(sapply(interv, fx, y = input)))
  colnames(output) = "NInRange"
  output
}
