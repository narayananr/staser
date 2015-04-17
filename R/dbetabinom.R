betabinom <- function(alpha,beta,hi,tot) {
  #copied from http://en.wikipedia.org/wiki/Beta-binomial_distribution
  #lchoose(n,k) + lbeta(k+alpha, n-k+beta) - lbeta(alpha, beta)
  lchoose(tot,hi) + lbeta(hi+alpha, tot-hi+beta) - lbeta(alpha, beta)
}
