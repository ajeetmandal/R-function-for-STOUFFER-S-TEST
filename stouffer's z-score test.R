#=================== STOUFFER'S-TEST function =========================================
# Read more her : https://en.wikipedia.org/wiki/Fisher%27s_method
Stouffer.test<- function(p, w) {  # p: is a vector of p-values # w: Vector of weights 
  if (missing(w)) {
    w <- rep(1, length(p))/length(p)
  } else {
    if (length(w) != length(p))
      stop("Length of p and w must equal!")
  }
  logp<- log(p)     ## log transform pvalues
  Zi <- -qnorm(logp, log.p= TRUE) ## Work in log space
  Z  <- sum(w*Zi)/sqrt(sum(w^2))
  p.val <- -pnorm(Z, log.p= TRUE)
  out.p<- c(Z_score= Z, Stouffers_p.value= p.val)
  #out.p<- c(Stouffers_p.value= p.val)
  return(out.p)
}
#===================================================================================
