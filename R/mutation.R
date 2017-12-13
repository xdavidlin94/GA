# Mutation
bitFlipMutation <- function(child, rate, nFeatures){
  #' Mutation
  #'
  #' Produces a new offspring of same length given a offspring using
  #' mutation genetic operator
  #'
  #' @param child vector of chromosomes in binary
  #' @param rate numeric detailing rate of altering a feature
  #' @param nFeatures the number of features, which determines length of each vector
  #'
  #' @details Altering each feature separately to produce a new
  #' offspring given the rate and the child
  #'
  #' @return Mutated child with bits flipped with change  =  rate

  booleanArray <- runif(nFeatures) < rate

  child[booleanArray] <- as.numeric(!child[booleanArray])

  return(child)
}

