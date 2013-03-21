## Octagon-1: the first program example in Octagon paper [Mine06]

## TARGET: at the end of while, -1 <= x <= 0, 0 <= y <= 101
## BOX: -1<=x<=0 , 0<=y
## PTOPE: may prove some properties but not others, depending from the
##        matrix chosen by the PCA/OSCA
## COMBINED: sometimes, but only with a low theta (0.80), otherwise some 
##           two's are generated in the resulting matrix which are bad.
## PTOPE ad-hoc: may prove some properties but not others, depending from
##               the choice of the matrix. 
## --- COMBINED ad-hoc: OK, with matrix PC1= [ 1 0 ], PC2 = [ 1 1 ]                 
## --- OCTAGON: OK

## ICA PTOPE:  may prove some properties but not others, depending from the
## matrix chosen 
## ICA COMBINED: it seems it always works, but only with low theta

octagon1 = function() {
  x = 100
  y = 0
  while (x>=0) {
    x = x-1
    if (brandom())
      y = y+1 
  }
}


