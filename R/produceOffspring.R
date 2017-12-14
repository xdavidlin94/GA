produceOffspring <- function(parents, nFeatures, populationSize,
                             geneticOperator) {
  #' Produce Offspring Vectors
  #'
  #' Uses genetic operators and mutation to produce new offsprings
  #'
  #' @param parents a matrix of parent vectors that will be used to produce offsprings
  #' @param nFeatures the number of features, which determines length of each vector
  #' @param populationSize size of initial population (# of vectors)
  #' @param geneticOperator genetic operator to use
  #'
  #' @return matrix containing the new population vectors to be used in next iteration

  # initializing new population matrix
  nParents <- length(parents[,1])
  newPopulation <- matrix(nrow = 2*choose(nParents,2), ncol = nFeatures)

  counter <- -1
  # looping through all possible combinations of nParents vectors
  for (i in 1:(nParents-1)) {
    for (j in (i+1):nParents) {

      counter <- counter + 2

      # crossover / genetic operators
      children  =  geneticOperator(parents[i,], parents[j,], nFeatures)

      # Storing child vectors after mutation for new population
      newPopulation[counter,] <- bitFlipMutation(children[[1]], rate = 1/nFeatures, nFeatures)
      newPopulation[counter+1,] <- bitFlipMutation(children[[2]], rate = 1/nFeatures, nFeatures)
    }
  }
  return(newPopulation)
}

