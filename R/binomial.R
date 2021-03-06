#' Binomial test for allele specific expression in a single sample
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

binomial <- function(data_mat){
     pvals = apply(data_mat,1,
             function(x){binom.test(round(x[1]),round(x[2]),alternative="two.sided")$p.value})
     return(pvals)
}

#nodSum.np= apply(nodCnt.np,1,sum)
#totSum.np = apply(totCnt.np,1,sum)
#bpvals.np= apply(cbind(nodSum.np,totSum.np),1,
#                 function(x){binom.test(round(x[1]),round(x[2]),alternative="two.sided")$p.value})
