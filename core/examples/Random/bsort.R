## Primo esempio in CousotC78: bubblesort
##
## BOX:
##   while esterno: 0<=b , 0<=j, 0<=t
##   while interno: 1<=b , 1<=j, 0<=t
##   prima di accessi ad array: 2<=b, 1<=j , 0<=t
## ROTATED BOX (k=100:1): tutte le componenti includono tmp e questo mi fa perdere precisione
## COMBINED BOX (k=100:1): come BOX.. tutte le componenti includono tmp e questo mi fa perdere precisione
##
## ROTATED BOX / COMBINED BOX with GENERATORS: come sopra, tutte le componenti includono tmp
##
## inverse ROTATED BOX with GENERATORS:
##   while esterno: niente
##   while interno: 1<=b
##   prima di accessi ad array: 1<=b
## inverse COMBINED BOX with GENERATORS: come BOX
##
## ROTATED BOX (k=100:1)  EXCLUDING tmp:
##   while esterno: niente
##   while interno: 1<=b
##   prima di accessi ad array: 1<=b
## ----- COMBINED BOX (k=100:1) EXCLUDING tmp:
##   while esterno: 0<=b<=100, 0<=j<=100, 0<=t<=99, 0<=j-t<=100
##   while interno: 1<=b<=100, 1<=j<=100, 0<=t<=99,  1<=j-t<=100
##   prima di accessi ad array: 2<=b<=100, 1<=j<=99, 0<=t<=98, 1<=j-t<=99 ###CHECK###
## ----- ROTATED BOX/COMBINED BOX with GENERATORS and EXCLUDING tmp: come il caso k=100:1
##
## ROTATED AD-HOC: scelgo gli assi guardando la sintassi (as esempio v1=b, v2=j-b, v3=t-j)
##   analyze.function(bubblesort,rotatedbox.domain(solve(matrix(c(1,0,0,-1,1,0,-1,0,1),nrow=3,byrow=TRUE),
##                                                 vars=c("b","j","t"))))
##   while esterno:  0<=b-t<=100 , b-j<=100, -100<=j-t
##   while interno:  1<=b,  1<=b-t, 0<=b-j
##   prima di accessi ad array:  1<=b, 1<=b-t, 1<=b-j
## COMBINED AD-HOC: semplice combinazione di ROTATED AD-HOC e BOX
##   while esterno: 0<=b , 0<=j , 0<=t , 0<=b-t<=100 , b-j<=100, -100<=j-t
##   while interno:  1<=b , 1<=j , 0<=t , 1<=b-t , 0<=b-j
##   prima di accessi ad array: 2<=b , 1<=j , 0<=t , 1<=b-t , 1<=b-j
##
## --- INTERPROC: 
##       box: come noi 
##       octagon: 1 <= b <= 100, 0<= j <= 100, 0<= t <= 99,  1<= b+j <= 199, 0 <= b-t <= 100,  1<= b+t <= 198, 0<= j-t <= 100
##       accessi array corretti!

if (FALSE) "
/* Versione INTERPROC */

var b: real, j:real, t:real;
begin
  j=0;
  t=0;
  b=100;
  while b>=1 do 
    j=1;
    t=0;
    while j<=b-1 do
      if brandom then   /* k[j]>k[j+1] */
        /* tmp = k[j+1]
        k[j+1] = k[j]
        k[j]=tmp */
        t=j;
      endif;
      j=j+1;
    done;
    if t==0 then
      halt;
    endif;
    b=t;
  done;
end
"

bubblesort = function(k) {
  b = 100
  while (b>=1) {
    j=1
    t=0
    while (j<=(b-1)) {
      if (k[j]>k[j+1]) {
        tmp = k[j+1]
        k[j+1] = k[j]
        k[j]=tmp       
        t=j
      }
      j=j+1
    }
    if (t==0) return(k)
    b=t
  }
  return(k)
}

bubblesort.cases =  function() {
  list(list(k=c(1:98,100,99)),list(k=100:1),list(k=c(100,rep(1,99))))
}

list(bubblesort, bubblesort.cases())
