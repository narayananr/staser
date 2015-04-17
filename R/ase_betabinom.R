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
~
