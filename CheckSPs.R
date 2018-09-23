#compares two vectors and gives you elements that are present only in one of the vectors 
#a, b: vectores to be compared

CheckSPs <- function(a, b){ 
  ab <- c(a, b)
  ba <- c(b, a)
  la <- length(a)
  lb <- length(b)
  notINa <- b[!duplicated(ab)[(la + 1):(la + lb)]]
  notINb <- a[!duplicated(ba)[(lb + 1):(la + lb)]]
  return(list("species not in first list" = notINa, "species not in second list" = notINb))
}

