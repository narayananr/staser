#' Beta-Binomial test for testing allele specific expression
#' using multiple samples
#' 
#' A more detailed description of what the function is and how
#' it works. It may be a paragraph that should not be separated
#' by any spaces. 
#'
#' @param inputParameter1 A description of the input parameter \code{inputParameter1}
#' @param inputParameter2 A description of the input parameter \code{inputParameter2}
#'
#' @return output A description of the object the function outputs 
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works


dbetabinom <- function(alpha,beta,hi,tot) {
  #copied from http://en.wikipedia.org/wiki/Beta-binomial_distribution
  #lchoose(n,k) + lbeta(k+alpha, n-k+beta) - lbeta(alpha, beta)
  lchoose(tot,hi) + lbeta(hi+alpha, tot-hi+beta) - lbeta(alpha, beta)
}


myloglik <- function(alpha,beta, his, tots) {
  # just sum the log-densities
  cntTab = cbind(his,tots)
  #sum(dbetabinom(alpha,beta, k, n=2000))
  sum(apply(cntTab,1,function(x){dbetabinom(alpha,beta, x[1], x[2])}))
}


myloglik.h0 <- function(alpha, his, tots) {
  # just sum the log-densities
  cntTab = cbind(his,tots)
  #sum(dbetabinom(alpha,beta, k, n=2000))
  sum(apply(cntTab,1,function(x){dbetabinom(alpha,alpha, x[1], x[2])}))
}

aseByBetaBinom <- function(dataMat,totMat){
  pvals=c()
  for (i in 1:nrow(dataMat)){
    his= as.numeric(round(dataMat[i,]))
    tot=as.numeric(round(totMat[i,]))
    f1 <- function(par) -myloglik(par[1], par[2],his,tot)
    m1=optim(par, f1)
    f0 <- function(par) -myloglik.h0(par,his,tot)
    m0=optim(par[1], f0, method="Brent",lower=0,upper=max(m1$par+5))
    pval=1-pchisq(2*(-m1$value + m0$value), df = 1)
    pvals=c(pvals,pval)
  }
  return(pvals)
}
