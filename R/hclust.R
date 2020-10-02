#my implementation of HCLUST
#' Perform Hierarchical Clustering
#'
#' Perform agglomerative hierarchical clustering on a given data matrix and
#' create K clusters
#' @param data.matrix input data
#' @param K number of clusters
#'
#' @return a vector of cluster assignments
#' @export
#'
#' @examples HCLUST(iris[,1:4], 3)
HCLUST <- function(data.matrix, K)
{
  #compute pairwise distance matrix
  distanceMatrix <- pairwiseDistance(data.matrix)

  #set diagonal to NA to ignore in in minimum calculatios
  diag(distanceMatrix)=NA


  #initialize cluster vector and put each data point in its own cluster
  numClust <- nrow(data.matrix)
  clusters <- 1:nrow(data.matrix)

  #while(number of clusters is greater than K)
  while(nrow(distanceMatrix) > K)
  {
    #find minimum entry in distance matrix
    minLoc<- which(distanceMatrix == min(distanceMatrix [ !is.na(distanceMatrix) ]),
                   arr.ind = TRUE)
    minLoc <- minLoc[1,]

    #minLoc <- which(distanceMatrix == min(distanceMatrix [ distanceMatrix > 0]), arr.ind = TRUE)
    #arr.ind = TRUE)

    #minLoc <- which(x == min(distanceMatrix), arr.ind=TRUE)
    #relabel points in larger # cluster to smaller one
    for(i in 1:length(clusters))
    {

      if(isTRUE(clusters[i] == max(minLoc)))
      {
        clusters[i] <- min(minLoc)
      }
      if(isTRUE(clusters[i] > max(minLoc)))
      {
        clusters[i] <- clusters[i] - 1
      }
    }
    #recalculate new distances with new cluster row = min(combined cls)
    distanceMatrix[min(minLoc),] <- pmin(distanceMatrix[min(minLoc),],
                                         distanceMatrix[max(minLoc),])
    #delete unneccessary row and col
    distanceMatrix <- distanceMatrix[-max(minLoc), -max(minLoc)]


  }
  #end while

  return(clusters)
}

#'  Compute matrix of pairwise Euclidean distance between observations
#'
#' @param data.matrix Data matrix of n observations
#'
#' @return nxn matrix with Euclidean distance between observation a and
#' observation b located at \[ a,b \] and \[ b,a \]
#' @export
#'
#' @examples pairwiseDistance(as.matrix(iris.dt[,1:4]))
pairwiseDistance <- function(data.matrix) {
  #initialize matrix
  distMatrix <- matrix(data = NA, nrow = nrow(data.matrix),
                       ncol = nrow(data.matrix))
  #for(number of data points)
  for(pointOne in 1:nrow(data.matrix))
  {
    for(pointTwo in 1:nrow(data.matrix))
    {
      distVec <- data.matrix[pointOne, ] - data.matrix[pointTwo,]
      distVec <- distVec^2
      dist <- sum(distVec)
      dist <- sqrt(dist)
      distMatrix[pointOne, pointTwo] <- dist

    }

  }

  return(distMatrix)
}
