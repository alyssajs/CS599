test_that("HCLUST returns vector of length n where n is number of data points", {
  expect_equal(length(HCLUST(iris[,1:4], 3)), nrow(iris[,1:4]))
})
