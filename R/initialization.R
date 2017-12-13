initializeVectors <- function(populationSize, nFeatures) {
  #' Initialize Population Vectors
  #'
  #' Randomly initialized n = populationSize vectors of length  =  nFeatures
  #'
  #' @param populationSize size of initial population (# of vectors)
  #' @param nFeatures the number of features, which determines length of each vector
  #'
  #' @return Matrix of size populationSize x nFeatures
  #' denoting initialized population vectors

  # initializing randomly
  startingVals <- sample(0:1, populationSize * nFeatures, replace = TRUE)

  # converting to matrix
  population <- matrix(
    startingVals, nrow = populationSize, ncol = nFeatures
    )

  return(population)
}
