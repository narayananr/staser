#' Beta-Binomial model for testing allele specific expression using replicates
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
dbetabinom <- function(alpha,beta,hi,tot) {
  #copied from http://en.wikipedia.org/wiki/Beta-binomial_distribution
  #lchoose(n,k) + lbeta(k+alpha, n-k+beta) - lbeta(alpha, beta)
  lchoose(tot,hi) + lbeta(hi+alpha, tot-hi+beta) - lbeta(alpha, beta)
}
