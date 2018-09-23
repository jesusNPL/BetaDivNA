#### ARGUMENTS ####

# gridshp: grid as shapefile (or "SpatialPolygonsDataFrame" class). 
# At the end of the post I present a version of the function which does not need 
# this grid (and the xfeature and yfeature arguments).

# comp: community data matrix (species occurrence on each grid cell).

# xfeature: number of the feature within the grid shapefile corresponding to 
# the longitude.

# yfeature: number of the feature within the grid shapefile corresponding to 
# the latitude.

# radius: the radius that define the maximum distance to select neighbor cells.

# phylotree (optional): phylogenetic tree ("phylo" class). 
# It can also be a "phylo" class functional dendrogram.

# phylobeta (optional): to calculate or not phylogenetic beta diversity 
# (see "phylo.beta.pair" function in "betapart" package) instead of the usual 
# beta diversity (see "beta.pair" function in "betapart" package). Default is F.

# index: to calculate "sorensen" or "jaccard". Default is "sorensen".

###################
betaSpatial <- function(data, radius, phylotree, phylobeta = F, index = "sorensen"){
  
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
  
  for(i in 1:length(data[, 1])){
    svMisc::progress(i, max.value = length(data[, 1]))
    adj <- select.window(xf = data[i, 1], yf = data[i, 2], radius, xydata = data)[, -c(1, 2)]
    if(phylobeta == F){
      res <- beta.pair(adj, index.family = index)
    } else if(phylobeta == T){
      res <- phylo.beta.pair(adj, phylotree, index.family = index)
    }
    mean_turnover[i] <- mean(as.matrix(res[[1]])[2:length(as.matrix(res[[1]])[, 1]), 1], na.rm = TRUE)
    mean_nestedness[i] <- mean(as.matrix(res[[2]])[2:length(as.matrix(res[[2]])[, 1]), 1], na.rm = TRUE)
    mean_beta[i] <- mean(as.matrix(res[[3]])[2:length(as.matrix(res[[3]])[, 1]), 1], na.rm = TRUE)
  }
  return(data.frame(cell = row.names(data), mean_turnover, mean_nestedness, mean_beta))
}

