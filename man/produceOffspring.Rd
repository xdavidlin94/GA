% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/produceOffspring.R
\name{produceOffspring}
\alias{produceOffspring}
\title{Produce Offspring Vectors}
\usage{
produceOffspring(parents, nFeatures, populationSize, geneticOperator)
}
\arguments{
\item{parents}{a matrix of parent vectors that will be used to produce offsprings}

\item{nFeatures}{the number of features, which determines length of each vector}

\item{populationSize}{size of initial population (# of vectors)}

\item{geneticOperator}{genetic operator to use}
}
\value{
matrix containing the new population vectors to be used in next iteration
}
\description{
Uses genetic operators and mutation to produce new offsprings
}
