library(testthat)
# source("geneticOperators.R")
context("uniformCrossover()")

test_that("Invalid input", {
  expect_error(uniformCrossover(), 'argument "nFeatures" is missing, with no default')
})


test_that("Offspring's length stays the same after Crossover",{
  parent1 <- sample(0:1, 10, replace = T)
  parent2 <- sample(0:1, 10, replace = T)
  nFeatures <- length(parent1)

  children <- uniformCrossover(parent1, parent2, nFeatures)

  expect_equal(class(children),"list")
  expect_equal(lengths(children),c(length(parent1),length(parent2)))
})


test_that("Crossover produced different offsprinngs and is random",{
  parent1 <- sample(0:1, 20, replace = T)
  parent2 <- sample(0:1, 20, replace = T)
  nFeatures <- length(parent1)

  children1 <- uniformCrossover(parent1, parent2, nFeatures)
  children2 <- uniformCrossover(parent1, parent2, nFeatures)

  expect_false(identical(children1[[1]],children2[[1]]))
  expect_false(identical(children1[[2]],children2[[2]]))
  expect_false(identical(parent1[[1]],children1[[1]]))
  expect_false(identical(parent1[[2]],children1[[2]]))
  expect_false(identical(parent2[[1]],children2[[1]]))
  expect_false(identical(parent2[[2]],children2[[2]]))
})
