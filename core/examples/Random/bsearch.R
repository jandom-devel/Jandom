## Second example in CousotC76: binary search
## BOX
##   invariante a fine while:  1<=lwb<=101 , 0<=upb<=100 , 1<=m<=100 :
##                             (-100<=m-lwb<=99 , -100<=upb-m<=99, -101<=upb-lwb <=99)
##
## ROTATED BOX: in generale fa schifo, ma vedi casi speciali sotto
## COMBINED BOX: in generale come box, ma vedi casi speciali sotto
##
## ROTATED BOX (k=62, r=1:100): -1 <= upb-lwb <= 48.5, -50.5 <= 2*m-upb-lwb <= 50.5
##                              (-25.75 <= m-lwb <= 49.5)                       
## ----- COMBINED BOX (k=62, r=1:100): come BOX + ROTATED BOX
##
## ROTATED BOX with GENERATORS (10): fa schifo, non trova niente
## ---- COMBINED BOX with GENERATOR (10): come BOX, più -100<=upb-lwb<=98
##
## INVERSE ROTATED BOX with GENERATORS (10): fa schifo, non trova niente
## NVERSE COMBINED BOX with GENERATOR (10): come BOX 
##
## ROTATED AD HOC: non ottengo nulla
## ---- COMBINED AD HOC:
##    analyze.function(bsearch,combinedbox.domain(solve(matrix(c(-1,0,1,0,1,-1,0,0,1),byrow=TRUE,nrow=3))), vars=c("lwb","upb","m"))
##    invariante a fine while:  come BOX più -99<=m-lwb<=99 , -99<=upb-m<=99
##
## ICA con theta = 0.98 => come PCA
## ----- ICA con theta = 0.80 => come BOX più -1<=m-lwb<=99 , -1<=upb-m<=99 , -2<=upb-lwb<=99 
## INTERPROC: si ottiene lo stesso risultato che con la Combined ad Hoc

if (FALSE) "
/* Versione INTERPROC */

var lwb:real, upb:real, m:real, inloop:int;
begin
  lwb=1;
  upb=100;
  m = 0;
  inloop=1;
  while ((lwb <= upb) and (inloop==1)) do
    m = (lwb+upb)/2;
    if (brandom) then /* k == r[m] */
      inloop=0; 
    else 
       if (brandom) then  /* k < r[m] */
         upb = m-1;
       else 
         lwb = m+1;
       endif;
    endif;
   done;
end
"

bsearch = function (k, r) {
	lwb=1
	upb=100  
	while (lwb <= upb) {
		m = (lwb+upb) %/% 2    # %/% is integer division
		if (k==r[m]) {
			return(TRUE)
		} else if (k<r[m]) {
			upb = m-1
		}
		else  {
			lwb = m+1    
		}
	}  
	return(FALSE)
}

bsearch.cases = function (n) {
	params=list()
	for (i in 1:n)
	   params=c(params,list(list(k=round(100/n*i),r=1:100)))
	params=c(params,list(list(k=0,r=1:100),list(k=101,r=1:100)))
	return(params)
}

list(bsearch,bsearch.cases(10))

