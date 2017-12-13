library(testthat)
# source("geneticOperators.R")
context("laplaceCrossover()")

test_that("Offspring's length stays the same after Crossover",{
  parent1 <- sample(0:1, 10, replace = T)
  parent2 <- sample(0:1, 10, replace = T)
  nFeatures <- length(parent1)

  children <- laplaceCrossover(parent1, parent2, nFeatures)

  expect_equal(class(children),"list")
  expect_equal(lengths(children),c(length(parent1),length(parent2)))
})


test_that("Crossover produced different offsprinngs with their parents",{
  parent1 <- sample(0:1, 20, replace = T)
  parent2 <- sample(0:1, 20, replace = T)
  nFeatures <- length(parent1)

  children <- laplaceCrossover(parent1, parent2, nFeatures)

  expect_false(identical(children[[1]], parent1))
  expect_false(identical(children[[1]], parent2))
  expect_false(identical(children[[2]], parent1))
  expect_false(identical(children[[2]], parent2))
})


test_that("Crossover didn't change the features",{
  parent1 <- rep(0, 20)
  parent2 <- rep(1, 20)
  nFeatures <- length(parent1)

  children <- arithmeticCrossover(parent1, parent2, nFeatures)

  expect_equal(sum(children[[1]])+sum(children[[2]]), nFeatures)
})
