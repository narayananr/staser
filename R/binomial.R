# Binomial test for testsing allele specific expression
# in a single sample
nodSum.np= apply(nodCnt.np,1,sum)
totSum.np = apply(totCnt.np,1,sum)
bpvals.np= apply(cbind(nodSum.np,totSum.np),1,
                 function(x){binom.test(round(x[1]),round(x[2]),alternative="two.sided")$p.value})
