#' Bayesian Changepoint for Binomial Data 
#'
#' This package provides a binomial variant of bayesian changepoint analysis.
#'
#' @author
#' Neal Fultz \email{njf@@zestfinance.com},
#' 
#' @name binomialbcp
#' @docType package
#' @useDynLib binomialbcp
#' @import Rcpp
#' @import Matrix
NULL

#' Kansas Presidential Election Data
#' 
#' A data set of presidential election in the state of Kansas from 1864 to 2012. Votes are in 1000s.
#' 
#' 
#' @format A data frame with 38 obs of 6 variables:
#' \describe{
#'   \item{Year}{year of election}
#'   \item{TotalVote}{Number of Votes in 1000s}
#'   \item{Democrat}{}
#'   \item{Republican}{}
#'   \item{Independent}{}
#'   \item{Other}{}
#' }
#' 
#' @docType data
#' @name kansas
#' @source Dave Leip's Atlas of US Presidential Elections \url{http://uselectionatlas.org/RESULTS/compare.php?year=2012&fips=20&f=1&off=0&elect=0&type=state}
NULL
