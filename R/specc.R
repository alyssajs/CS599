


#my spectral clustering function
#' Performs Spectral Clustering
#'
#' Performs spectral clustering using K-nearest neighbors (where K is passed in
#' as a parameter) using Euclidean distances. Uses K-means clustering on the
#' eigenvectors.
#'
#' @param data.dt Data table of observations
#' @param numClust Desired number of clusters
#' @param numEigen Desired number of eigenvalues used
#' @param numNeighbors Number of neighbors to choose in K-nearest neighbor
#' computation
#'
#' @return Vector of cluster assignments.
#' @export
#'
#' @examples
#' library(data.table)
#' set.seed(1)
#' halfcircle <- function(r, center = c(0, 0), class, sign, N=150, noise=0.5) {
#'  angle <- runif(N, 0, pi)
#'  rad <- rnorm(N, r, noise)
#'  data.table(
#'    V1 = rad * cos(angle) + center[1],
#'    V2 = sign * rad * sin(angle) + center[2]
#'    )
#' }
#' X.dt <- rbind(
#'  halfcircle(4, c(0, 0), 1, 1),
#'  halfcircle(4, c(4, 2), 2, -1))
#'
#'result <- SPECC(X.dt, 2, 5, 5)
SPECC <- function(data.dt, numClust, numEigen, numNeighbors)
{
  #calculate distance matrix
  dist.mat <- pairwiseDistance(data.dt)

  #calculate similarity matrix
  sim.mat <- similarity(data.dt)

  #calculate adjacency matrix
  adjacency.mat <- adjacency(dist.mat, sim.mat, numNeighbors)

  #calculate degree matrix
  degree.mat <- degree(adjacency.mat)

  #calculate laplacian
  laplacian.mat <- laplacian(adjacency.mat, degree.mat)

  #calculate eigenvalues/vectors
  eigens <- eigen(laplacian.mat)

  #create matrix with rows consisting of vectors
  eigens <- eigens$vectors

  #get eigenvectors corresponding to smallest eigenvalues
  eigens <- eigens[, (numEigen+1):ncol(eigens)]


  #use kmeans to cluster based on eigenvalues
  kmeansRes <- stats::kmeans(eigens, centers=numClust)

  return(kmeansRes$cluster)
}

#similarity matrix computation using
#' Computes radial-kernel gram matrix to use as similarity matrix with scale
#' parameter = 1 and Euclidean distance
#'
#' @param data.matrix Matrix of observations
#'
#' @return Similarity matrix between observations
#' @export
#'
#' @examples similarity(iris[,1:4])
similarity <- function(data.matrix)
{
  distMatrix <- pairwiseDistance(data.matrix)
  similarity <- matrix(data=NA, nrow=nrow(data.matrix), ncol=nrow(data.matrix))
  for(pointOne in 1:nrow(data.matrix))
  {
    for(pointTwo in 1:nrow(data.matrix))
    {
      similarity[pointOne, pointTwo] <- exp(-1 * distMatrix[pointOne, pointTwo]^2)
    }
  }

  return(similarity)
}

#compute the adjacency matrix given the similarity matrix
#' Computes adjacency matrix given similarity matrix. Each observation is connected
#' to its K-nearest neighbors and the similarity measures as provided by the
#' similarity matrix are used as edge weights.
#'
#' @param distance.mat Matrix of pairwise distances between observations
#' @param similarity.mat Matrix of pairwise similarities between observations
#' @param K Number of nearest neighbors to connect each observation to
#'
#' @return Adjacency matrix
#' @export
#'
#' @examples distMat <- pairwiseDistance(iris[, 1:4])
#' similarityMat <- similarity(iris[, 1:4])
#' adjacency(distMat, similarityMat, 3)
adjacency <- function(distance.mat, similarity.mat, K)
{
  adjacency.mat <- matrix(0, nrow=nrow(distance.mat), ncol=nrow(similarity.mat))
  #for each point, calculate the K-nearest neighbors
  for(point in 1:nrow(similarity.mat))
  {
    nearest <- nearestNeighbors(distance.mat, point, K)

    #edge weights come from similarity matrix
    for(i in nearest)
    {
      adjacency.mat[i, point] <- similarity.mat[i, point]
      adjacency.mat[point, i] <- similarity.mat[point, i]
    }

  }

  return(adjacency.mat)
}

#compute K-nearest neighbors
#' Computes K-nearest neighbors of given observation
#'
#' @param distance.mat Matrix of pairwise distances of observations
#' @param point Given observation to find nearest neighbors of
#' @param K number of nearest neighbors
#'
#' @return Vector of nearest neighbors
#' @export
#'
#' @examples distMat <- pairwiseDistance(iris[, 1:4])
#' neighborsVec <- nearestNeighbors(distMat, 3, 2)
nearestNeighbors <- function(distance.mat, point, K)
{
  #get row of distances from our point
  distances <- distance.mat[point, ]

  #sort these distances
  distances <- sort(distances, index.return=TRUE)

  #get the indices sorted by distance
  neighbors <- distances$ix

  #get the K-nearest neighbors (ignoring first entry since that is the point itself)
  neighbors <- neighbors[2:(K+1)]

  return(neighbors)

}

#compute the degree matrix
#' Cpmpute matrix with degree of each vertex on the diagonal
#'
#' @param adjacency.mat Adjacency matrix
#'
#' @return Matrix with degrees of vertex i on \[ i, i \]
#' @export
#'
#' @examples
#' adjacency.mat = adjacency(pairwiseDistance(iris[,1:4]),
#' similarity(iris[,1:4]))
#' degree(adjacency.mat)
degree <- function(adjacency.mat)
{
  degree <- matrix(0, nrow=nrow(adjacency.mat), ncol=nrow(adjacency.mat))
  for(point in 1:nrow(adjacency.mat))
  {
    degree[point, point] <- sum(adjacency.mat[point, ])
  }

  return(degree)
}

#graph Laplacian computation
laplacian <- function(adjacency.mat, degree.mat)
{
  laplacian.mat <- degree.mat - adjacency.mat
  return(laplacian.mat)
}


#euclidean distance matrix calculation

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
      dist <- sum(distVec^2)
      dist <- sqrt(dist)
      distMatrix[pointOne, pointTwo] <- dist

    }

  }

  return(distMatrix)
}
