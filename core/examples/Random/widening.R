# Prova widening

if (FALSE) "
/* Versione INTERPROC di Wide */
var x:real,y:real;
begin
  x=0;
  y=0;
  while (x<100) do
     x=x+1;
     if (brandom) then
       y=x;
     endif;
  done;
end
"

wide = function(k) {
	x = 0
	y = 0
	while (x<100) {
		x=x+1
		if (k[x]<=0) y=x
	}		
}
