## Octagon-5: the fifth program example in Octagon paper [Mine06]
##            Absolute Value

## TARGET: inside the last if: -69 <= x <= 69, 0 <= y <=69
## BOX: -100 <= x <= 100, 0 <= y <=69
## PTOPE: -69<=x<=69 , 0<=y<=138 
## COMBO: OK
## ---- OCTAGON: OK

## ICA like PCA, but it also works with theta=0.98 while PCA does not work in this case
if (FALSE) "
/* Versione INTERPROC */
var x:real, y:real;
begin
  assume x>=-100 and x<=100;
  y=x;
  if y<=0 then 
    y=-y;
  endif;
  if y<=69 then
    skip;
  endif;
end
"

absval = function (x) {	
  assume(x>=-100 && x <=100)
  y=x
  if (y<=0) y = -y
  if (y<=69) y = y 
}

absval.cases = list(
     list(x=-100), list(x=-10), list(x=0), list(x=10), list(x=100)
)

list(absval, absval.cases)

