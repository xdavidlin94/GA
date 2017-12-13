library(testthat)
context("initializeVectors()")

test_that("Errors on invalid populationSize and nFeatures input",{
  expect_error (initializeVectors(), 'argument "populationSize" is missing, with no default')
  expect_error(initializeVectors(40), 'argument "nFeatures" is missing, with no default')
  expect_error(initializeVectors(-40,-20), "invalid 'nrow' value \\(< 0\\)")
  expect_error(initializeVectors(-40,20), "invalid 'size' argument")
  expect_error(initializeVectors("40","20"), 'non-numeric argument to binary operator')
  expect_warning(initializeVectors(40.5,20.5), 'data length \\[830\\] is not a sub-multiple or multiple of the number of rows \\[40\\]')
})

test_that("Population only contain binary integer numbers 0 or 1",{
  population<-initializeVectors(40,20)
  expect_equal(class(population),"matrix")
  expect_equal(class(population[,2]),"integer")
  expect_equal(max(unique(population[,2])), 1)
  expect_equal(min(unique(population[,2])),0)
})

test_that("Correct dimension of population when input is valid",{
  expect_equal(dim(initializeVectors(40,20)),c(40,20))
  expect_equal(dim(initializeVectors(20,10)),c(20,10))
  expect_equal(dim(initializeVectors(10,20)),c(10,20))
})

test_that("Repeated initialization with same populationSize and nFeatures doesn't return same population",{
  population1<-initializeVectors(40,20)
  population2<-initializeVectors(40,20)
  expect_false(identical(population1,population2))
})

