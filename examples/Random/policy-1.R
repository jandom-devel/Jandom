# This example appear in the paper
# "A Policy Iteration Algorithm for Computing Fixed Points in
#  Static Analysis of Programs"
#
# Other examples in http://www.di.ens.fr/~goubault/Politiques
#
# Not very interesting. It is only used to check performance of interval
# analysis.

policy1 <- function()
{
  i=1
  j=10
  while (j>=i) {
    i = i+2;
    j = -1+j;
  }  
}

