## Octagon-4: the fourth program example in Octagon paper [Mine06]

## TARGET: at the end, -1 <= i <= 0, 17 <= x <= 18
## BOX: -1 <=i <=0 , 1 <= x 
## --- PTOPE: OK
## --- COMBINED: OK
## --- OCTAGON: OK

## ICA like PCA

if (FALSE) "
/* Versione INTERPROC */

var x:real, i:real;
begin
  i = 16;
  x = 1;
  while i>0 do
    x = x+1;
    i = i-1;
 done;
end
"

octagon4 = function() {
  i = 16
  x = 1
  while (i>0) {
    x = x+1
    i = i-1
  }
}

