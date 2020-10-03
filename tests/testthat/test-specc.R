test_that("SPECC returns vector of cluster assignments of length n, where n is
          the number of data points", {
  expect_equal(length(SPECC(iris[,1:4], 3, 3, 2)), nrow(iris[,1:4]))
})

test_that("similarity matrix returns nxn matrix where n is the number of clusters",
          {
            expect_equal(nrow(similarity(iris[,1:4])), nrow(iris[,1:4]))
            expect_equal(ncol(similarity(iris[,1:4])), nrow(iris[,1:4]))
          })

test_that("adjacency matrix returns numerical matrix",
          {
            expect_true(is.numeric(adjacency(pairwiseDistance(iris[,1:4]),
                                             similarity(iris[,1:4]), 2)))
          })
test_that("nearest neighbors returns a numeric vector of specified
          length",
          {
            distance.mat = pairwiseDistance(iris[,1:4])
            expect_equal(length(nearestNeighbors(distance.mat,1,2)), 2)
            expect_true(is.numeric(nearestNeighbors(distance.mat,1,2)))
          })
test_that("degree matrix returns matrix of same size as adjacency matrix",
          {
            adjacency.mat = adjacency(pairwiseDistance(iris[,1:4]),
                                      similarity(iris[,1:4]), 2)
            degree.mat = degree(adjacency.mat)
            expect_equal(nrow(adjacency.mat),
                         nrow(degree.mat))
            expect_equal(ncol(degree.mat),
                         ncol(adjacency.mat))
          })
test_that("laplacian returns numeric matrix",
          {
            adjacency.mat = adjacency(pairwiseDistance(iris[,1:4]),
                                      similarity(iris[,1:4]), 2)
            degree.mat = degree(adjacency.mat)
            expect_true(is.numeric(laplacian(
              adjacency.mat, degree.mat)))
          })
test_that("pairwiseDistance returns matrix of size nxn where n is number of data points", {
  expect_equal(nrow(pairwiseDistance(iris[,1:4])), nrow(iris[,1:4]))
  expect_equal(ncol(pairwiseDistance(iris[, 1:4])), nrow(iris[,1:4]))
})
