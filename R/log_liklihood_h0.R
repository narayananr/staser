myloglik.h0 <- function(alpha, his, tots) {
  # just sum the log-densities
  cntTab = cbind(his,tots)
  #sum(dbetabinom(alpha,beta, k, n=2000))
  sum(apply(cntTab,1,function(x){dbetabinom(alpha,alpha, x[1], x[2])}))
}
