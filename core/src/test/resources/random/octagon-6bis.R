## Octagon-6bis: the sixth program example in Octagon paper [Mine06]
##                RATE LIMITER

## The while condition has been changed to TRUE. The use of brandom makes
## the executions traces too small.
## In a previous bugged version of RANDOM this was relevant, but now it isn't:
## PCA/OSCA does not find good axes in any case.

## ICA: singular matrix

ratelim = function() {
  y=0
  while (TRUE) {
    x = trunc(runif(1,-128,129))
    assume(x>=-128 && x<=128)
    d = trunc(runif(1,0,17))
    assume(d>=0 && d<=16)
    s = y
    r = x-s
    y=x
    if (r<=-d) y=s-d
    if (d<=r) y=s+d
  }
}

