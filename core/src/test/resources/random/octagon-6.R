## Octagon-6: the sixth program example in Octagon paper [Mine06]
##            RATE LIMITER

## TARGET: at the beginning of while: -128 <= y <= 128
## BOX: no
## PTOPE: no
## COMBO: no
## --- OCTAGON: -144 <= y <= 144 using widening with threshold

## PTOPE ad-hoc 1: y <= 160 using delayed widenings and matrix
##     y  x  d s r
## PC1 1  0  0 0 0
## PC2 0  1  0 0 0
## PC3 0  0  1 0 0
## PC4 0 -1  0 1 1
## PC5 0  0 -1 1 0 

## PTOPE ad-hoc 2: -160 <= y using delayed widenings and matrix
##     y  x  d s r 
## PC5 0  0  1 1 0  (other rows as above)

## COMBO ad-hoc: -128 <= y <= 128 using delayed widenings and matrix 
##     y  x  d s r
## PC3 0  0  1 1 0  (other rows as PTOPE ad-hoc 1)

## ICA: singular matrix!

ratelim = function() {
  y=0
  while (brandom()) {
    x = trunc(runif(1,-128,129))
    assume(x>=-128 && x<=128)
    d = trunc(runif(1,0,17))
    assume(d>=0 && d<=16)
    s = y
    r = x-s
    y = x
    if (r<=-d) y = s-d
    if (d<=r) y = s+d
  }
}

ratelim.cases = function(n) {
  lapply(1:n,function(n) list())
}

list(ratelim, ratelim.cases(10))

