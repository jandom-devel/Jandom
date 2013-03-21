## Esempio di scansione della retta x + y = 0
##
## BOX:
##   invariante while: -11<=x<=10 , -10<=y<=11 (-21<=x+y<=21, -22<=x-y<=20)
##
## ROTATED BOX no SIMPLIFY:
##   invariante while: -1<=x<=127381033450515456 , -10<=y<=127381033450515456 ,
##                         -2.04112019628891e-15<=x+y<=254762066901030880, -22<=x-y<=20
## COMBINED BOX no SIMPLIFY:
##   invariante del while: -1<=x<=10 , -10<=y<=11, -2.04112019628891e-15<=x+y<=20, -2<=x-y<=20
##
## ----- both ROTATED BOX: 
##   invariante while:     -1<=x<=10, -10<=y<=1, 0<=x+y<=0, -2<=x-y<=20
## both COMBINED BOX:
##   invariante while: come ROTATED BOX
##
## INTERPROC: box come i nostri, octagon da gli stessi risultati del combined ad-hoc

if (FALSE) "
/* Versione INTERPROC */

var x:real, y:real;
begin
  x=10;
  y=-10;
  while x>y do
    x= x-1;
    y= y+1;
 done;
end
"

xyline=function() {
  x=10
  y=-10
  while(x>y) {
    x= x-1
    y= y+1
  }
}
