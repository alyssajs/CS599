test_that("HCLUST returns vector of length n where n is number of data points", {
  expect_equal(length(HCLUST(iris[,1:4], 3)), nrow(iris[,1:4]))
})
test_that("pairwiseDistance returns matrix of size nxn where n is number of data points", {
  expect_equal(nrow(pairwiseDistance(iris[,1:4])), nrow(iris[,1:4]))
  expect_equal(ncol(pairwiseDistance(iris[, 1:4])), nrow(iris[,1:4]))
})
