% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pi_ij.R
\name{pi_ij_replace}
\alias{pi_ij_replace}
\title{Calculate joint inclusion probability of unit $j$ and $h$ when sampling occurs with replacement}
\usage{
pi_ij_replace(N, n1, m_vec)
}
\arguments{
\item{N}{Population size.}

\item{n1}{An integer giving the initial sample size (e.g., a simple random sample).}

\item{m_vec}{Vector of values \eqn{m} for the set of units in a sample, of length \eqn{n1}. Each \eqn{m} value within the vector \code{m_vec} denotes the number of units satisfying the ACS criterion for the network \eqn{i} to which the unit belongs.}
}
\description{
Calculate joint inclusion probability of unit $j$ and $h$ when sampling occurs with replacement
}
