## Primo esempio in CousotC78 versione 2: bubblesort con dimensione parametrica
##
## BOX: come versione non parametrica
##
## ROTATED BOX (with and without GENERATORS):  come versione non parametrica, perché una delle componenti principali
##                                             è il vettore n
## COMBINED BOX (with and without GENERATIRS): vedi ROTATED BOX
## ROTATED BOX con GENERATORS e EXCLUDING tmp: niente, matrice PCA troppo complessa, con pochi zeri
## COMBINED BOX con GENERATORS e EXCLUDING tmp: come BOX
##
## inverse ROTATED BOX (no GENERATORS (100:1), WITH/WITHOUT tmp):
##  while esterno: 1<=n
##  while interno: 1<=n, 1<=b
##  prima array:   1<=n, 1<=b
## inverse COMBINED BOX (no GENERATORS (100:1), WITH tmp): come BOX
## inverse COMBINED BOX (no GENERATORS (100:1), WITHOUT tmp):
##  while esterno: 1<=n, 0<=b, 0<=j, 0<=t, t-j<=0
##  while interno: 1<=n, 1<=b, 1<=j, 0<=t, t-j<=-1
##  accesso array: 1<=n, 2<=b, 1<=j, 0<=t, t-j<=-1
## inverse ROTATED BOX (GENERATOS, without tmp): nulla
## inverse COMBINED BOX (GENERATOS, without tmp): come BOX
##
## ROTATED AD-HOC: aggiungo b-n agli assi del caso non parametrico
##   analyze.function(bubblesort.param,combinedbox.domain(solve(matrix(c(1,0,0,0,-1,1,0,0,-1,0,1,0,1,0,0,-1),
##                     nrow=4,byrow=TRUE))), vars=c("b","j","t","n"))
##   while esterno:  0<=b-t<=Inf
##   while interno:  1<=b<=Inf , 1<=n<=Inf , 1<=b-t<=Inf , 0<=b-j<=Inf , -Inf<=j-n<=0
##   prima di accessi ad array:  1<=b<=Inf , 1<=n<=Inf , 1<=b-t<=Inf , 1<=b-j<=Inf , -Inf<=j-n<=-1
## COMBINED AD-HOC: quasi una semplice combinazione di ROTATED AD-HOC e BOX, ma i vincoli 1<=n sono migliorati!!
##   while esterno: 0<=b<=Inf , 0<=j<=Inf , 0<=t<=Inf, 0<=n<=Inf, 0<=b-t<=100 , -Inf<=b-j<=100
##   while interno:  1<=b<=Inf , 1<=j<=Inf , 0<=t<=Inf, 1<=n<=Inf, 1<=b-t<=Inf , 0<=b-j<=Inf, -Inf<=j-n<=0
##   prima di accessi ad array: 2<=b<=Inf , 1<=j<=Inf , 0<=t<=Inf, 1<=n<=Inf, 1<=b-t<=Inf , 1<=b-j<=Inf , -Inf<=j-n<=-1
## INTERPROC: box come noi, octagon come COMBINED AD HOC ma in più ricava j-t>=1 che noi non possiamo
##            ottenere  perché j-t è lin. dipendente da b-j e b-t. Suppongo che noi possiamo ricavare altre
##            proprietà scegliendo un vettore di base ruotata diverso al posto di "b"
##
## Sia INTERPROC che COMBINED AD-HOC possono dimostrare che gli accessi agli array sono corretti

if (FALSE) "
/* Versione INTERPROC */

var b: real, j:real, t:real, n:real;
begin
  assume(n>=1);
  b=n;
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

bubblesort.param = function(n,k) {
  assume(n>=1)
  b = n
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

bubblesort.param.cases1 = function(n) {
  params=list()
  for (i in 2:n)
    params=c(params,list(list(n=i,k=i:1)))
  return(params)
}

bubblesort.param.cases2 = function(n) {
  params=list()
  for (i in 1:n)
    params=c(params,list(list(n=i,k=i:1),list(n=i,k=1:i),list(n=i,k=rep(1,i))))
  return(params)
}

bubblesort.param.cases3 = function(n) {
  params=list()
  for (i in 2:n)
    params=c(params,list(list(n=i,k=rnorm(i))))
  return(params)
}

list(bubblesort.param, bubblesort.param.cases3(10))
