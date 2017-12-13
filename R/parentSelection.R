calcFitness <- function(population, y, df, fitnessFUN, family) {
  #' Calculates the fitness of a population
  #'
  #' Calculates fitness of a population using AICs of regression results
  #'
  #' @param population matrix of population vectors
  #' @param y vector of dependent variable in regression
  #' @param df dataframe containing all covariates used for regression
  #' @param fitnessFUN function handle for fitness function to be used
  #' @param family a description of the error distribution
  #' and link function to be used in the model.
  #'
  #' @return fitness values (AICs)

  # Obtains subsets of covariate matrix to be used in regression
  xSubset <- apply(
    population, 1, function(x){ df[as.logical(x)] }
    )

  # Applys regression to all subsets of data determined by population vectors

  regFun <- function(x) {
      glm(y~., data = x, family = family)
  }

  models <- lapply(
    xSubset, regFun
    )

  summaries <- lapply(models, summary)

  # Obtains AICs
  AICs <- sapply(models, fitnessFUN)

  return(AICs)
}



selection <- function(AICs, k, population, populationSize) {
  #' Selection of highest fitness vectors
  #'
  #' Selects the best fit vectors used to produce offsprings.
  #' Uses roulette wheel to select
  #'
  #' @param AICs fitness values (AICs of regression models)
  #' @param k parameter used to scale ranking. Must be between 1 & 2
  #' @param population matrix of population vectors
  #'
  #' @return a matrix of parent vectors that will be used to produce offsprings

  # roulette wheel selection
  # determining orders of AICs values; lower AIC equals higher rank
  orderedAICs <- order(AICs)
  ranks <- k * orderedAICs

  # ranks of parents which will be used to produce offspring
  nParents <- floor(sqrt(populationSize))

  parentsRanks <- sample(orderedAICs, size  =  nParents, prob = ranks)

  parentsIndices <- which(orderedAICs %in% parentsRanks)

  # Uses parentsIndices to select parents used for producing offspring
  parents <- population[parentsIndices, ]

  return(parents)
}

