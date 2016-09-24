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
#' @examples
#'  data(kansas)
#'  binomialbcp(kansas$Democrat, kansas$TotalVote)

binomialbcp <- function(x, n, prior=list(a=1, b=1), control=binomialbcp.control()) {
  ret <- rcpp_bbbcp_gibbs(x, n, prior, control)
  cpt <- sparseMatrix(ret[[2]], ret[[1]], index1 = FALSE, dims=c(length(n),ret[[3]])) #implicit transpose
  `class<-`(list(cpt=cpt, x=x, n=n, call=match.call()), "binomialbcp")
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
#' This function makes autocomplete work for \code{binomialbcp}.
#'
#' @export
#'
#' @param mcmc.iterations  number of iterations
#' @param     mcmc.burnin  burnin samples
#' @param       mcmc.thin  thining interval
#' @param         verbose  if zero, no debug messages; if 1, once per mcmc step; if >1, log each Gibbs sample
#' 

binomialbcp.control <- function(mcmc.iterations=100, mcmc.burnin=50, mcmc.thin=5, verbose=0) {
  if(verbose > 1) message("Verbose > 1 not implemented yet")
  as.list(environment())
}


#' Plot a Binomial BCP sample
#' 
#' @param x a binomialbcp sample
#' @param ... passed on to plot
#' 
#' @export
plot.binomialbcp <- function(x, ...) {
  dat <- summary(x)
  opar <- par(mfrow=2:1, yaxs="i")
  on.exit(par(opar))
  
  par(mar=c(0.5,5,2,0.5), xaxt='n')
  plot(dat[[2]], type='l', ylim=0:1, ylab=expression(hat(p)[t]), 
       xlab="", ...)
  points(x$x / x$n, pch=4)
  grid()
  
  par(mar=c(3,5,0.5,0.5), xaxt='s')
  plot(dat[[1]], type='l', ylim=0:1, ylab=expression(P(C[i] *"|"* X)), 
       main=NULL, mar=c(3,2,0,0.2), mai=c(1,1,0,0))
  grid()
  
}
  
#' Summarize a Binomial BCP sample
#' 
#' Average over the MCMC samples to estimate changepoints and proportions.
#' 
#' @param object a binomialbcp sample
#' @param ... (unused)
#' 
#' @return a dataframe containing the probability of a change point and estimated proportion for each time point.
#' 
#' @method summary binomialbcp
#' @export
summary.binomialbcp <- function(object, ...) {
  p <- rowMeans( apply(object$cpt, 2, function(r) {g <- cumsum(r); ave(object$x,g) / ave(object$n, g) }  ) ) 
  cp <- rowMeans(object$cpt)
  data.frame("cpt%"=cp, p=p)
}

# Log Likelihood of a draw
L <- function(x, n, cpt) {
  g <- cumsum(cpt)
  p <- ave(x,g) / ave(n, g)
  L <- sum( dbinom(cpt, 1, mean(cpt), log=TRUE),lchoose(n, x) , 
            log(p) * x , log1p(-p) * (n - x), na.rm=TRUE )
}

