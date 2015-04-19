#' R function to compute log liklihood for Beta Binomial model under null hypothesis
#'
#' A more detailed description of what the function is and how
#' it works. It may be a paragraph that should not be separated
#' by any spaces.
#'
#' @param data matrix with first column one of the allele counts and the second column total allele \code{inputParameter1}
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
log_liklihood_h0 <- function(alpha, his, tots) {
  # just sum the log-densities
  cntTab = cbind(his,tots)
  #sum(dbetabinom(alpha,beta, k, n=2000))
  sum(apply(cntTab,1,function(x){dbetabinom(alpha,alpha, x[1], x[2])}))
}
