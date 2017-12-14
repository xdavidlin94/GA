# main select function
select <- function(
  data,
  yColumn,
  fitnessFUN = match.fun(AIC),
  k = 2,
  geneticOperator = match.fun(singlePtCrossover),
  family = match.fun(gaussian),
  nIterations = 200
) {
  #' Genetic Algorithm for Feature Selection
  #'
  #' Genetic Algorithm for Feature Selection. User must pass in a dataframe and integer
  #' specifying which column to use as the dependent variable. The function also allows
  #' for different fitness functions, genetic operators, and selective pressure to be applied
  #' To use GLM, specific a non-gaussian family.
  #'
  #' @param data dataframe containing dependent variable (y) and all covariates
  #' @param yColumn integer specifying which column of data is dependent variable
  #' @param fitnessFUN fitness function to use. Default is AIC
  #' @param k selective pressure for ranking. Default is 2
  #' @param geneticOperator genetic operator to use. Default is single point crossover.
  #' The function must take 2 parents and the number of features as
  #' input and return two children
  #' @param family a description of the error distribution and
  #' link function to be used in the model. Default is gaussian (standard lm()). To use
  #' glm(), specify a different family such as poisson(), etc...
  #' @param nIterations integer specifying number of iterations before stopping
  #'
  #' @return Binary vector detailing optimal features to use
  #'
  #' @examples bestFeatures <- select(data = fakeData, yColumn = 1, fitnessFUN = AIC, k = 2,
  #' geneticOperator = singlePtCrossover, family = gaussian)

  y  =  data[[yColumn]]
  df  =  data[-yColumn]

  # initializing new population matrix
  nFeatures <- length(df)
  populationSize <- 2 * nFeatures
  population <- initializeVectors(populationSize, nFeatures)

  # initializes min AIC
  currentMinAIC <- .Machine$integer.max

  iterations <- 0
  while (iterations < nIterations) {
    iterations <- iterations + 1

    # Calculates fitness of current population
    AICs <- calcFitness(population = population, y = y, df = df,
                        fitnessFUN = fitnessFUN, family = family)

    # If fitness is highest thus far, store the vector
    if (min(AICs) < currentMinAIC) {

      currentMinAIC <- min(AICs)
      index <- which(AICs  ==  min(AICs))

      # Incase we get ties for best fitness
      if (length(index) !=  1) {
        index <- index[1]
      }

      # Storing
      bestFeatures <- population[index,]
    }

    # Obtaining parents used for producing offspring
    parents <- selection(AICs, k, population, populationSize)

    # Obtaining new population through crossover and mutation
    population <- produceOffspring(parents, nFeatures, populationSize,
                                   geneticOperator)
  }
  return(bestFeatures)
}

