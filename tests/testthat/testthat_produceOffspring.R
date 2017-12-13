library(testthat)
context("produceOffspring()")

test_that("Errors on invalid input",{
  parents<-matrix(sample(0:1, 6 * 20, replace = TRUE), nrow = 6, ncol = 20)
  nFeatures<-20
  populationSize<-40
  expect_error(produceOffspring(), 'argument "parents" is missing, with no default')
  expect_error(produceOffspring(parents,nFeatures,populationSize,geneticOperator = Crossover), "object 'Crossover' not found")
  expect_error(produceOffspring(parents,nFeatures,populationSize,geneticOperator = "Crossover"), 'could not find function "geneticOperator"' )
  # The number of columns of input parents should be equal to the number of features
  # When number of feature is less than the number of column for parents
  expect_warning(produceOffspring(parents,nFeatures=10,populationSize,geneticOperator = uniformCrossover))
  # When number of feature is more than the number of column for parents, there will be NA appears in the output
  newPopulation_result<-produceOffspring(parents,nFeatures=30,populationSize,geneticOperator = uniformCrossover)
  expect_equal(produceOffspring(parents,nFeatures=30,populationSize,geneticOperator = uniformCrossover)[1,nFeatures+1], as.double(NA))
})

test_that("Output of produceOffspring function is in the correct format for uniformCrossover",{
  parents<-matrix(sample(0:1, 6 * 20, replace = TRUE), nrow = 6, ncol = 20)
  nFeatures<-20
  populationSize<-40
  newPopulation_result<-produceOffspring(parents,nFeatures,populationSize,geneticOperator = uniformCrossover)
  # The output of produceOffspring function is a matrix
  expect_equal(class(newPopulation_result), "matrix")
  # The output of produceOffspring function contains only binary number 0 or 1
  expect_equal(length(unique(newPopulation_result[,2])),2)
  expect_equal(max(unique(newPopulation_result[,2])), 1)
  expect_equal(min(unique(newPopulation_result[,2])),0)
  # Output new population size is less than the original population size
  expect_lt(dim(newPopulation_result)[1], populationSize)
  # Output number of features is equal to the original number of features
  expect_equal(dim(newPopulation_result)[2],nFeatures)
})

test_that("Output of produceOffspring function is in the correct format for single point crossover",{
  parents<-matrix(sample(0:1, 6 * 20, replace = TRUE), nrow = 6, ncol = 20)
  nFeatures<-20
  populationSize<-40
  newPopulation_result<-produceOffspring(parents,nFeatures,populationSize,geneticOperator = singlePtCrossover)
  # The output of produceOffspring function is a matrix
  expect_equal(class(newPopulation_result), "matrix")
  # The output of produceOffspring function contains only binary number 0 or 1
  expect_equal(length(unique(newPopulation_result[,2])),2)
  expect_equal(max(unique(newPopulation_result[,2])), 1)
  expect_equal(min(unique(newPopulation_result[,2])),0)
  # Output new population size is less than the original population size
  expect_lt(dim(newPopulation_result)[1], populationSize)
  # Output number of features is equal to the original number of features
  expect_equal(dim(newPopulation_result)[2],nFeatures)
})

test_that("Output of produceOffspring function is different for different choice of genetic operators",{
  parents<-matrix(sample(0:1, 6 * 20, replace = TRUE), nrow = 6, ncol = 20)
  nFeatures<-20
  populationSize<-40
  newPopulation_result1<-produceOffspring(parents,nFeatures,populationSize,geneticOperator = singlePtCrossover)
  newPopulation_result2<-produceOffspring(parents,nFeatures,populationSize,geneticOperator = uniformCrossover)
  expect_false(identical(newPopulation_result1,newPopulation_result2))
})

test_that("Repeated running of produceOffspring function with same inputs creates different new population",{
  parents<-matrix(sample(0:1, 6 * 20, replace = TRUE), nrow = 6, ncol = 20)
  nFeatures<-20
  populationSize<-40
  newPopulation_result1<-produceOffspring(parents,nFeatures,populationSize,geneticOperator = singlePtCrossover)
  newPopulation_result2<-produceOffspring(parents,nFeatures,populationSize,geneticOperator = singlePtCrossover)
  expect_false(identical(newPopulation_result1,newPopulation_result2))
})

