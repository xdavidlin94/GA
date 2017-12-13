uniformCrossover <- function(parent1, parent2, nFeatures) {
  #' Uniform Crossover
  #'
  #' Produces two new offspring of same length given two parents using
  #' uniform crossover genetic operator
  #'
  #' @param parent1,parent2 vector of parent chromosomes in binary
  #' @param rate numeric detailing rate of obtaining parent1's traits
  #' @param nFeatures the number of features, which determines length of each vector
  #'
  #' @details Altering each feature separately by the rate to
  #' produce a new offspring for each parent
  #'
  #' @return a list of 2 child vectors produced from uniform crossover

  # initialize child vectors
  child1 <- rep(0, nFeatures)
  child2 <- rep(0, nFeatures)

  # uses boolean vector to access vectors
  rate <- 0.5
  booleanArray <-  runif(nFeatures) < rate

  # determing which bits gets transfered to which child
  child1[booleanArray] <- parent1[booleanArray]
  child1[!booleanArray] <- parent2[!booleanArray]

  child2[booleanArray] <- parent2[booleanArray]
  child2[!booleanArray] <- parent1[!booleanArray]

  return(list(child1,child2))
}

# single point(cut&splice) crossover
singlePtCrossover <- function(parent1, parent2, nFeatures){
  #' Single Point Crossover
  #'
  #' Produces two new offsprings of same length given two parents using
  #' single point crossover genetic operator
  #'
  #' @param parent1,parent2 vector of parent chromosomes in binary
  #' @param nFeatures the number of features, which determines length of each vector
  #'
  #' @details Randomly choose an interger from 1 to nFeatures to be
  #' the crossover point and swap the tails of its two parents to produce
  #' two new offsprings
  #'
  #' @return a list of 2 child vectors produced from uniform crossover

  # initalize child vectors
  child1 <- rep(0, nFeatures)
  child2 <- rep(0, nFeatures)

  # determining split index
  i <- sample(1:nFeatures, 1)

  # allocating bits before and after split point to each child
  child1[1:i] <- parent1[1:i]
  child2[1:i] <- parent2[1:i]

  if (i !=  nFeatures) {
    child1[(i+1):nFeatures] <- parent2[(i+1):nFeatures]
    child2[(i+1):nFeatures] <- parent1[(i+1):nFeatures]
  }

  return(list(child1,child2))
}

arithmeticCrossover <- function(parent1, parent2, nFeatures)
{
  #' Arithmetic Crossover
  #'
  #' Produces two new offsprings of same length given two parents using
  #' single point crossover genetic operator
  #'
  #' @param parent1,parent2 vector of parent chromosomes in binary
  #' @param nFeatures the number of features, which determines length of each vector
  #'
  #' @details Linearly combines parents to produce two new offsprings according to
  #' weighted recombination of vectors
  #'
  #' @return a list of 2 child vectors produced from arithmetic crossover
  
  # initialize child vectors
  child1 <- rep(0, nFeatures)
  child2 <- rep(0, nFeatures)
  
  #Choose 'a' as random weighting factor
  a <- 0
  child1 <- a*parent1 + (1-a)*parent2
  child2 <- a*parent2 + (1-a)*parent1
  return(list(child1,child2))
}

laplaceCrossover <- function (parent1, parent2, nFeatures, locn = 0, scl = 0.35)
{
  #' Laplace Crossover - Source: Proceedings of the International Conference
  #' on Soft Computing for Problem Solving [Deep and Thakur]
  #'
  #' Produces two new offsprings of same length given two parents using
  #' laplace crossover genetic operator
  #'
  #' @param parent1,parent2 vector of parent chromosomes in binary
  #' @param nFeatures the number of features, which determines length of each vector
  #' @param locn the location parameter
  #' @param scl the scaling parameter - most suitably 0.35 for integers
  #'
  #' @details  Generates child vectors based on Lapalce distribution within
  #' the parent vectors
  #'
  #' @return a list of 2 child vectors produced from Laplace crossover
  
  # Initialize child vectors
  child1 <- rep(0, nFeatures)
  child2 <- rep(0, nFeatures)
  
  # Generate uniformly distributed random numbers x and y in the interval [0,1]
  x <- runif(nFeatures,min=0,max=1)
  y <- runif(nFeatures,min=0,max=1)
  
  # Generate beta, which follows the Laplace distribution using the formula:
  # beta <- a + b*log(y) if x <= 0.5
  # beta <- a - b*log(y) if x > 0.5
  beta <- locn + ifelse(x > 0.5, scl*log(y), -scl*log(y))
  betaAbs <- beta*abs(parent1 - parent2)
  
  #Generate children based on parallel max and min for parent vectors
  child1 <- pmin(pmax(parent1 + betaAbs, which.min(parent1)), which.max(parent1))
  child2 <- pmin(pmax(parent2 + betaAbs, which.min(parent2)), which.max(parent2))
  
  # If the crossover generates a child that violates the box constraint, then the child vector member is
  # randomly assigned a value in the max-min interval
  return(list(child1,child2))
}

