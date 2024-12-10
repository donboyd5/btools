
test_that("ns returns sorted names", {
  # Test with data.frame
  expect_equal(
    ns(iris),
    c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", "Species")
  )
  
  # Test with named vector
  x <- c(b = 2, a = 1, c = 3)
  expect_equal(ns(x), c("a", "b", "c"))
  
  # Test with named list
  y <- list(foo = 1, bar = 2)
  expect_equal(ns(y), c("bar", "foo"))
})

test_that("ns handles edge cases", {
  # Test with unnamed object
  x <- 1:3
  expect_equal(ns(x), character(0))
  
  # Test with empty named object
  y <- numeric()
  names(y) <- character()
  expect_equal(ns(y), character(0))
})
