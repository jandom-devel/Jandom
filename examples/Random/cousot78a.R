##
## Second Example in CousotC78a
##
## Invariant at the end of while: 2j-i <= -2 , j>=0 , 2j+i >= 6
##
## BOX:                            [4<=i], [0<=j], 4<=i+j,             8<=2*i+j,   4<=2*j+i 
## both ROTATED BOX (theta=0.80):   4<=i,        , [5<=i+j], [3<=i-j], 9<=2*i+j,                            7<=2*i-j
## both COMBINED BOX (theta=0.80): [4<=i], [0<=j], [5<=i+j], [3<=i-j], 9<=2*i+j,   4<=2*j+i,                7<=2*i-j
##
## ROTATED BOX (theta=0.98):       4<=i,                       3<=i-j, [9<=2*i+j],             [2*j-i<=-2], 7<=2*i-j
## inv ROTATED BOX (theta=0.98):   4<=i,           5<=i+j,             [9<=2*i+j], [6<=2*j+i],             [7<=2*i-j]
## ---- COMBINED BOX (theta=0.98): [4<=i], [0<=j], [4<=i+j],   3<=i-j, [9<=2*i+],  4<=2*j+i,   [2*j-i<=-2], 7<=2*i-j
## inv COMBINED BOX (theta=0.98):  [4<=i], [0<=j], 5<=i+j,              9<=2*i+j,  [6<=2*j+i],             [7<=2*i-j]
##
## ICA
## does not work: singular matrix
##
## With ad-hoc matrix a1=solve(matrix(c(0,1,1,2),nrow=2))
## ROTATED BOX:   -Inf<=i<=Inf, 1<=+j<=Inf , 6<=2*j+i
## COMBINED BOX:  4<=i<=Inf , 0<=j<=Inf , 6<=2*j+i<=Inf 
##
## With ad-hoc matrix a2=solve(matrix(c(0,-1,1,2),nrow=2))
## ROTATED BOX: 2<=i<=Inf , 0<=j<=Inf , 2<=2*j+i<=Inf , -Inf<=2*j-i<=-2
## COMBINED BOX:  4<=i<=Inf , 0<=j<=Inf , 4<=2*j+i<=Inf , -Inf<=2*j-i<=-2 
##
## With ad-hoc matrix a=solve(matrix(c(-1,1,2,2),nrow=2))
## ROTATED BOX: 4<=i<=Inf , -Inf<=j<=Inf , 6<=2*j+i<=Inf , -Inf<=2*j-i<=-2
## COMBINED BOX: 4<=i<=Inf , 0<=j<=Inf , 6<=2*j+i<=Inf , -Inf<=2*j-i<=-2
##
## OCTAGONS: same as COMBINED BOX (theta=pi/4)
## POLYHEDRA: true invariant
## CONSTRAINTS: 0 <= j, 2*j-i<=-2

if (FALSE) "
/* Versione INTEPROC */
var i:real, j:real;
begin
  i=2;
  j=0;
  while true do
    if i*i==4 then
      i=i+4;
    else 
      j=j+1;
      i=i+2;
    endif;
  done;
end
"

cousot78a = function() {
  i=2
  j=0
  while (TRUE) {
    if (i*i==4)
      i=i+4
    else {
      j=j+1
      i=i+2
    }
  }
}

