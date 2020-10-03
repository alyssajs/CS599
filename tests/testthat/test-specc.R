test_that("SPECC returns vector of cluster assignments of length n, where n is
          the number of data points", {
  expect_equal(length(SPECC(iris[,1:4], 3, 3, 2)), nrow(iris[,1:4]))
})

test_that("similarity matrix returns nxn matrix where n is the number of clusters",
          {
            expect_equal(length(similarity(iris[,1:4])), nrow(iris[,1:4]))
          })
