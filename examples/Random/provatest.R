### Prova della implementazione operazione select, e confronto con APRON

if (FALSE) "
Versione INTERPROC

proc testtry () returns (res: int) var x: int, y:int, z:int;
begin
  assume x >= 0 and y >= 0 and x <= 1 and y<=1;
  assume z >= 0 and z <= 5;
  if -x-y+z+1<=0 then
     res=1;
  else
     res=0;
  endif;
end

var r:int;
begin
 r = testtry();
end
"


test.try = function() {
  assume(x,0,1)
  assume(y,0,1)
  assume(z,0,5)
  if ( -x - y +  z + 1<=0)
    res=1
  else
    res=0  
}
