
BetaDivPhyloTraitSpatial <- function(data, radius, traits, phylotree, phylobeta = FALSE, index = "sorensen") {
  
  if ( ! ("rgdal" %in% installed.packages())) {install.packages("rgdal", dependencies = T)}
  if ( ! ("rgeos" %in% installed.packages())) {install.packages("rgeos", dependencies = T)}
  if ( ! ("picante" %in% installed.packages())) {install.packages("picante", dependencies = T)}
  if ( ! ("betapart" %in% installed.packages())) {install.packages("betapart", dependencies = T)}
  if ( ! ("CommEcol" %in% installed.packages())) {install.packages("CommEcol", dependencies = T)}
  if ( ! ("svMisc" %in% installed.packages())) {install.packages("svMisc", dependencies = T)}
  
  library(rgdal)
  library(rgeos)
  library(picante)
  library(betapart)
  library(CommEcol)
  
  mean_turnover <- numeric(length(data[, 1]))
  mean_nestedness <- numeric(length(data[, 1]))
  mean_beta <- numeric(length(data[, 1]))

  for (i in 1:length(data[, 1])) {
    svMisc::progress(i, max.value = length(data[, 1]))
    adj <- select.window(xf = data[i, 1], yf = data[i, 2],
                         radius, xydata = data)[, -(1:2), drop = FALSE]
    if (ncol(adj) == 1) {
      mean_turnover[i] <- 0
      mean_nestedness[i] <- 0
      mean_beta[i] <- 0
    } else {
      if (!phylobeta) {
        res <- functional.beta.pair(adj, traits, index.family = index)
      }
      if(phylobeta) {
        res <- phylo.beta.pair(adj, phylotree, index.family = index)
      }
      a <- as.matrix(res[[1]])[, 1]
      b <- as.matrix(res[[2]])[, 1]
      c <- as.matrix(res[[3]])[, 1]
      mean_turnover[i] <- mean(a[2:length(a)], na.rm = TRUE)
      mean_nestedness[i] <- mean(b[2:length(b)], na.rm = TRUE)
      mean_beta[i] <- mean(c[2:length(c)], na.rm = TRUE)
    }
  }
  result <- data.frame(mean_turnover, mean_nestedness, mean_beta)
  return(result)
}
