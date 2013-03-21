# This example appear in the paper
# "A Policy Iteration Algorithm for Computing Fixed Points in
#  Static Analysis of Programs"
#
# Other examples in http://www.di.ens.fr/~goubault/Politiques
#
# Policy iteration may prove that, at the end of the example, 
# it is the case that j <= 120. Box, ptopes, octagon, polyhedra
# cannot prove it.

# ICA: singular


if (FALSE) "
/* Interproc versione */
var i:real, j:real, k:real;
begin
  i = 0;
  k = 9;
  j = -100;
  while (i<=100) do
    i = i+1;
    while (j<20) do
      j = i+j;
    done;
    k = 4;
    while (k<=3) do
      k = k+1;
    done;
  done;
end
"

policy2 <- function()
{
  i = 0
  k = 9
  j = -100
  while (i<=100) {
    i = i+1
    while (j<20) 
      j = i+j
    k = 4
    while (k<=3)
      k = k+1
  }
}


