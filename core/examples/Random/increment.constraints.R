#
# Example INCREMENT in Colon, Sankaranarauana, Sipma 03
# "Linear Invariant Generation Using Non-Linear Constraint Solving"
# 
# Invariant at while: 1 <= i, 1<= k+i <= 2
# Intervals: 1 <= i, k <= 1
# Paralleolotopes: []
# * Combined: true invariant
# * Constraints: true invariant
# ICA: singulat matrix

increment = function(k) {
  assume(0 <= k && k <= 1)
  i=1
  j=1
  while (1==1) {
    i=i+1
    j=j+k
    k=k-1
  }
}

list(increment, list(list(k=0), list(k=1), list(k=0.5)))
