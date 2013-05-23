## First example in CousotC76: increment
## Si ottiene lo stesso con tutti i domini: 1<=i<=11 come invariante del while
## INTERPROC: tutto come per la nostra versione

if (FALSE) "
/* Versione INTERPROC */
var i:real;
begin
  i = 1;
  while (i<10) do
    i = i+1;
  done;
end
"

incr=function () {
	i=1
	while (i<10)
		i = i+1
}