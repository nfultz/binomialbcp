
test <- lapply(1:5, function(segment) {
  p <- runif(1)
  len <- sample(200, 1)
  cbind(t(replicate(len, {n=rbinom(1, 200, .6); x=rbinom(1, n, p); c(n=n,x=x) })), 
        p, segment)
})
test <- as.data.frame(do.call(rbind, test))
