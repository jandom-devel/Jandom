##
## Third example in CousotC78: heapsort
##
## BOX:
##  while interno: 2<=n , 1<=l, 2<=r,  1<=i, 2<=j, 0<=continue<=1
##  accessi array: non sicuri
##
## ROTATED BOX with GENERATORS(2,5): NULLA (problemi di precisione)
## COMBINED BOX with GENERATORS(2,5) : come BOX (problemi di precisione)
##
## ROTATED BOX with GENERATORS(2,10): 0<=continue<=1
## COMBINED BOX with GENERATORS(2,10): come BOX 
## ROTATED BOX ICA with GENERATORS(2,10): 0 <= continue <= 1, 2<=r
## COMBINED BOX ICA with GENERATORS(2,10): come BOX

## both ROTATED BOX with GENERATORS (1,5 without K): 
##   while interno: 4<=n+r, r-n<=0
##   accessi array: non sicuri
## both COMBINED BOX with GENERATORS (1,5 without K):
##   while interno: come BOX più ROTATED BOX
##   accessi array: non sicuri
##
## ROTATED BOX with RANDOM GENERATORS(2,7): NULLA
## COMBINED BOX with RANDOM GENERATORS(2,7): come BOX 
##
## ROTATED AD-HOC:
##    c=matrix(c(1,-2,0,0,0,0,0,1,0,-1,0,0,0,0,1,0,0,-1,0,0,0,1,-1,0,0,0,0,0,1,0,0,0,-1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1),byrow=TRUE,nrow=7)
##    analyze.function(heapsort,rotatedbox.domain(solve(c)),vars=c("n","l","i","j","r","k","continue"))
##    corrispondenti alle variabili n-2l, n-i, n-j, n-l, n-r, k, continue
##    while interno: -2<=n-2*l, 0<=n-i, 0<=n-l, 0<=n-r ,  0<=continue<=1
##    acessi array: non sicuri
## ----- COMBINED AD-HOC:  
##    while interno: ROTATED BOX + BOX
##    accessi array: tutti corretti
##
## INTERPROC:
##   box come noi
##   octagon riesce a dichiarare corretti alcuni accessi ad array e suppongo che potrebbe fare di più
##   se interproc funzionasse meglio.. in particolare dopo la divisione per 2 non capisce che l<=n

if (FALSE) "
/* versione INTERPROC */
/* simile a quella di esempio di INTERPROC, ma corretta */

var L:real,R:real,I:real,J:real, continue:real;
begin
  assume N>=1;
  L = N/2 +1;
  R = N;
  if (L>=2) then
    L = L-1; /* K = T[L]; */
  else
    /* K = T[R]; T[R] = T[1]; */
    R = R-1;
  endif;
  while (R>=2) do
    I = L;
    J = 2*I;
    continue = 1;
    while (J<=R and continue>0) do
      if (J<=R-1) then
        if /* T[J]<T[j-1] */ brandom then
	  J = J+1;
        endif;
      endif;
      if /* K>=T[J] */ brandom then
	continue=0;
      else
	/* T[I]=T[J]; */
	I = J;
	J = 2*J;
      endif;
    done;
    /* T[I] = K; */
    if (L>=2) then
      L = L-1; /* K = T[L]; */
    else
      /* K = T[R]; T[R]=T[1]; */
      R = R-1;
    endif;
    /* T[L] = K; */
  done;
end
"

heapsort = function(n,t) {
  assume(n>=2)
  l = (n %/% 2)+1
  r = n
  if (l >=2) {
    l = l-1
    k=t[l]
  } else {
    k=t[r]
    t[r]=t[1]
    r = r-1    
  }
  while (r >=2) {
    i=l
    j=2*i
    continue = 1
    while(j<=r && continue>0) {
      if (j<=r-1)
        if (t[j] < t[j+1])
          j=j+1
      if (k>=t[j])
        continue = 0
      else { 
        t[i]=t[j]
        i=j
        j=2*j
      }
    }
    t[i]=k
    if (l>=2) {
      l=l-1
      k=t[l]
    } else {
      k=t[r]
      t[r]=t[1]
      r=r-1
    }
    t[l]=k
  }
  return(t)
}

heapsort.cases = function(n1=2,n2) {
  params=list()
  for (i in n1:n2)
    params=c(params,list(list(n=i,t=i:1),list(n=i,t=1:i),list(n=i,t=rep(1,i))))
  return(params)
}

heapsort.cases.rand = function(n1=2,n2) {
   	params=list()
   	for (i in n1:n2)
		params=c(params,list(list(n=i,t=round(runif(i)*100))))
	return(params)
}

list(heapsort, heapsort.cases(2,10))

