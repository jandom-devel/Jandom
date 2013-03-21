## Octagon-3: the third program example in Octagon paper [Mine06]

## TARGET: at the end, n <= x <= n+1, 0 <= n
## BOX: 0 <= n
## PTOPE: n <= x
## --- COMBINED: n <= x <= n+1, 0 <= n 
## --- OCTAGON: come COMBINED

## ICA like PCA

if (FALSE) "
/* Versione INTERPROC */
var x: real, n: real;
begin
  x=0;
  assume n>=0;
  while x<n do
    x=x+1;
  done;
end
"

octagon3 = function(n) {
  x=0
  assume(n>=0)
  while (x<n)
    x = x+1
}

octagon3.cases = list (list(n=10),list(n=20), list(n=100))

list(octagon3, octagon3.cases )
