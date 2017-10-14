# create a new version of gcd that does not return message when
# any of the factor is prime
# 2017-10-14
library(schoolmath)
gcd_pep <- function(m, n){
  resp <- capture.output(gcd(m, n))
  gcd_value <- resp[length(resp)]
  gcd_num <- as.integer(strsplit(gcd_value," ")[[1]][2])
  return(gcd_num)
}