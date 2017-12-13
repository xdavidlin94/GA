library(testthat)
context("calcFitness()")

test_that("Errors on invalid input for calcFitness function",{
  populationSize<-40
  nFeatures<-20
  population<-initializeVectors(populationSize,nFeatures)
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
  x20 <- runif(n)
  e = rnorm(n)
  y = 1+ x1 + 2*x2 + x3 + 3*x4 + x5 + x6 + e
  df = data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
                  x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  expect_error(calcFitness(), 'argument "population" is missing, with no default')
  expect_error(calcFitness(population,y,df,fitnessFUN), 'argument "family" is missing, with no default')
  expect_error(calcFitness(population,y,df,fitnessFUN = AICCC, family=gaussian), "object 'AICCC' not found")
  expect_error(calcFitness(population,y,df,fitnessFUN = AIC, family=logistic), "object 'logistic' not found")
})

test_that("Output of calcFitness function is in the correct format",{
  populationSize<-40
  nFeatures<-20
  population<-initializeVectors(populationSize,nFeatures)
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
  x20 <- runif(n)
  e = rnorm(n)
  y = 1+ x1 + 2*x2 + x3 + 3*x4 + x5 + x6 + e
  df = data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
                  x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  AICs<-calcFitness(population,y,df,fitnessFUN = AIC, family=gaussian)
  #The length of calcFitness function output is equal to the population size
  expect_equal(length(AICs),populationSize)
  #The calcFitness output contains only numeric values
  expect_equal(class(AICs),"numeric")
})


context("selection()")

test_that("Errors on invalid input for selection function",{
  populationSize<-40
  nFeatures<-20
  population<-initializeVectors(populationSize,nFeatures)
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
  x20 <- runif(n)
  e = rnorm(n)
  y = 1+ x1 + 2*x2 + x3 + 3*x4 + x5 + x6 + e
  df = data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
                  x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  AICs<-calcFitness(population,y,df,fitnessFUN = AIC, family=gaussian)
  expect_error(selection(), 'argument "AICs" is missing, with no default')
  expect_error(selection(AICs, 1, population), 'argument "populationSize" is missing, with no default')
  # The choice of k cannot be a negative value
  expect_error(selection(AICs, -1, population, populationSize), 'negative probability')
})


test_that("Output of selection function is in the correct format",{
  populationSize<-40
  nFeatures<-20
  population<-initializeVectors(populationSize,nFeatures)
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
  x20 <- runif(n)
  e = rnorm(n)
  y = 1+ x1 + 2*x2 + x3 + 3*x4 + x5 + x6 + e
  df = data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
                  x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  AICs<-calcFitness(population,y,df,fitnessFUN = AIC, family=gaussian)
  parents<-selection(AICs, 1, population, populationSize)
  # The output of selection function is in the matrix form
  expect_equal(class(parents), "matrix")
  # The number of rows of the selection function output is equal to the lower range of the sqaure root of population size
  expect_equal(dim(parents)[1],floor(sqrt(populationSize)))
  # The number of columns of the selection function output is equal to the number of features
  expect_equal(dim(parents)[2], nFeatures)
  # The slection function output contains only binary number 0 or 1
  expect_equal(class(parents[,2]),"integer")
  expect_equal(max(unique(parents[,2])), 1)
  expect_equal(min(unique(parents[,2])),0)
  # Repeated running of selection function doesn't return same parents given the same population
  parents1<-selection(AICs, 1, population, populationSize)
  parents2<-selection(AICs, 1, population, populationSize)
  expect_false(identical(parents1,parents2))
  })


