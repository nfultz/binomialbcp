#' This is data to be included in my package
#'
#' @name test
#' @docType data
#' @author My Name \email{blahblah@@roxygen.org}
#' @references \url{data_blah.com}
#' @keywords data
test <- lapply(1:5, function(segment) {
  p <- runif(1)
  len <- sample(200, 1)
  data.frame(t(replicate(len, {n=rbinom(1, 200, .6); x=rbinom(1, n, p); c(n=n,x=x) })), 
        p, segment)
})
test <- do.call(rbind, test)
