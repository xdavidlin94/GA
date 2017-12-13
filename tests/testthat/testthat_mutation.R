library(testthat)
# source("mutation.R")
context("bitFlipMutation()")

test_that("Invalid input", {
  expect_error(bitFlipMutation(), 'argument "nFeatures" is missing, with no default')
  expect_error(bitFlipMutation(, , 20), 'argument "rate" is missing, with no default')
  expect_error(bitFlipMutation(, .5, 20), 'argument "child" is missing, with no default')
})


test_that("Offspring's length stay the same after Mutation",{
  child <- sample(0:1, 20, replace = TRUE)
  nFeatures <- length(child)
  rate <- 0.3
  new_child<- bitFlipMutation(child, rate, nFeatures)

  expect_equal(length(new_child),length(child))
})

test_that("Mutations happenes possibly",{
  child <- sample(0, 20, replace = TRUE)
  nFeatures <- length(child)
  rate <- 0.3
  new_child<- bitFlipMutation(child, rate, nFeatures)

  # sum of all zero is <= sum of all zero or with some ones
  expect_lte(sum(child), sum(new_child))
})

test_that("Mutations are randomly",{
  child <- sample(0:1,100,replace = TRUE)
  nFeatures <- length(child)
  rate <- 0.3

  test1 <- bitFlipMutation(child, rate, nFeatures)
  test2 <- bitFlipMutation(child, rate, nFeatures)

  expect_false(identical(test1,test2))
})


