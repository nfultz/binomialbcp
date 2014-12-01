#' Fit a beta-binomial bayesian changepoint model
#'
#' @export
#'
#' @param       x  the count of successes
#' @param       n  the size of each trial
#' @param   prior  a list of shape parameters for the beta prior
#' @param control  a binomialBCP.control object 
#'
#' @return a list containing the posterior samples of changepoints and expected values of proportions 

binomialbcp <- function(x, n, prior=list(a=1, b=1), control=bbbcp.control()) {
  ret <- rcpp_bbbcp_gibbs(x, n, prior, control)
  sparseMatrix(ret[[1]], ret[[2]], index1 = FALSE)
}

# R implementation
# function(x, n , prior=list(a=.5, b=.5), control=bbbcp.control()) {
#   
#   data <- data.frame(x=x, n=n, cpt=1)
#   
#   M <- control$mcmc.iterations
#   k <- nrow(data)
#   
#   changepoints <- matrix(0, nrow=k, ncol=M)
#   proportions  <- matrix(0, nrow=k, ncol=M)
#   
#   
#   lprior = lbeta(prior$a, prior$b)
#   l <- function(data){
#     x <- sum(data$x)
#     n <- sum(data$n)
#     lbeta(a=x + prior$a, b=n - x + prior$b) - lprior
#   }
#   h <- memoise(function(ngroups) {
#     a = ngroups+1
#     b = k-ngroups
#     #pbeta(.2, a, b, log.p = TRUE) + lbeta(a, b)
#     lbeta(a,b)
#   })
#   
#   for(i in 1:M){
#   
#     for(j in 2:k) {
#       data$cpt[j] = 0
#       
#       ngroups = sum(data$cpt)
#       
#       group = cumsum(data$cpt)
#       group  = (1:k)[group == group[j]  ]
# 
#       left  = group[group < j]
#       right = group[group >= j]      
#       
#       bothP  = h(ngroups) + l(data[group,])
#       splitP = h(ngroups+1) + l(data[left, ]) + l(data[right,])
#       
#       odds <- exp(bothP - splitP)
#       data$cpt[j] = rbinom(n = 1, size=1, p=1/(1+odds))
#       if(control$verbose)
#       message(sprintf("%d %d (pool: %f %f | split: %f %f %f) %6.5f", i, j,
#                       h(ngroups), l(data[group,]),h(ngroups + 1), l(data[left,]), l(data[right,]), 1/(1+odds)))
#       
#       
#       
#     }
#     
#     changepoints[,i] <- data$cpt
#     
#     group = cumsum(data$cpt)
#     proportions[,i]  <- ave(data$x, group) / ave(data$n, group)
#     
#     if(control$verbose > 0) 
#       message(sprintf("iteration %d:  %d groups, %f remaining", i, ))
#     
#   }
#   
#   ret <- list(changepoints=changepoints,
#                proportions=proportions   );
#   `class<-`(ret, "bbbcp")
# }

#' BB-BCP MCMC settings 
#'
#' This function makes autocomplete work.
#'
#' @export
#'
#' @param mcmc.iterations  number of iterations
#' @param     mcmc.burnin  burnin samples
#' @param       mcmc.thin  thining interval
#' @param         verbose  if zero, no debug messages; if 1, once per mcmc step; if >1, log each Gibbs sample
#' 

bbbcp.control <- function(mcmc.iterations=100, mcmc.burnin=500, mcmc.thin=5, verbose=FALSE) {
  as.list(environment())
}
