library(testthat)
context("select()")

test_that("Errors on invalid input",{
  n = 100
  x1 <- runif(n)
  x2 <- runif(n)
  x3 <- runif(n)
  x4 <- runif(n)
  x5 <- runif(n)
  x6 <- runif(n)
  x7 <- runif(n)
  x8 <- runif(n)
  x9 <- runif(n)
  x10 <- runif(n)
  x11 <- runif(n)
  x12 <- runif(n)
  x13 <- runif(n)
  x14 <- runif(n)
  x15 <- runif(n)
  x16 <- runif(n)
  x17 <- runif(n)
  x18 <- runif(n)
  x19 <- runif(n)
  x20_1 <- runif(n)
  
  e = rnorm(n)
  y = 1+ x1 + 2*x2 + x3 + 3*x4 + x5 + x6 + e
  fakeData = data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
                  x11,x12,x13,x14,x15,x16,x17,x18,x19,x20_1)
  # Wrong input to specify the y column
  expect_error(select(data=fakeData, yColumn = -1, fitnessFUN = AIC, k = 2, geneticOperator = singlePtCrossover, family = gaussian), 'attempt to select more than one element in get1index <real>')
  expect_error(select(data=fakeData, yColumn = 25, fitnessFUN = AIC, k = 2, geneticOperator = singlePtCrossover, family = gaussian), 'subscript out of bounds')
  
  # Wrong data input with characters in one of the column (characters will be automatically recognized as factor, but recognize only factors with at least two levels)
  x20_2 <- rep("NA",20)
  fakeData1 = data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
                         x11,x12,x13,x14,x15,x16,x17,x18,x19,x20_2)
  expect_error(select(data=fakeData1, yColumn = 1, fitnessFUN = AIC, k = 2, geneticOperator = singlePtCrossover, family = gaussian), "contrasts can be applied only to factors with 2 or more levels")

})

test_that("Output of select function is in the correct format",{
  n = 100
  x1 <- runif(n)
  x2 <- runif(n)
  x3 <- runif(n)
  x4 <- runif(n)
  x5 <- runif(n)
  x6 <- runif(n)
  x7 <- runif(n)
  x8 <- runif(n)
  x9 <- runif(n)
  x10 <- runif(n)
  x11 <- runif(n)
  x12 <- runif(n)
  x13 <- runif(n)
  x14 <- runif(n)
  x15 <- runif(n)
  x16 <- runif(n)
  x17 <- runif(n)
  x18 <- runif(n)
  x19 <- runif(n)
  x20_1 <- runif(n)
  
  e = rnorm(n)
  y = 1+ x1 + 2*x2 + x3 + 3*x4 + x5 + x6 + e
  fakeData = data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
                        x11,x12,x13,x14,x15,x16,x17,x18,x19,x20_1)
  bestFeature<-select(data=fakeData, yColumn = 1, fitnessFUN = AIC, k = 2, geneticOperator = singlePtCrossover, family = gaussian)
  # length of the best feature is 20 
  expect_equal(length(bestFeature), 20)
  # best feature consists possible values only 1 and 0
  expect_equal(max(unique(bestFeature)),1)
  expect_equal(min(unique(bestFeature)),0)
  
  })