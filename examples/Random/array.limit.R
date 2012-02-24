## Scansione array di n elementi
##
## BOX:
##   invariante: 1<=n, 1<=x
##
## ROTATED BOX, COMBINED BOX: come BOX il problema è che la PCA tiene separati gli assi x e k
##
## both ROTATED BOX with GENERATORS and SIMPLIFY : nulla, si potrebbe migliorare il trattamento di assume
## ----- both COMBINED BOX with GENERATORS and SIMPLIFY
##   invariante:  1<=n, 1<=x, x-n<=1
##
## ROTATED BOX AD HOC:  
##   analyze.function(array.limit,rotatedbox.domain(solve(matrix(c(-1,1,1,0),nrow=2, byrow=TRUE))), vars=c("n","x"))
##   invariante: 1<=n, x-n<=1
## COMBINED BOX AD HOC: combinazione di BOX e ROTATED BOX AD-HOC
##
## notare che se prendo assi molto simili (n, x-1.1*n) non riesco più ad avere
## nessuna informazione su x-n

array.limit=function(n) {
  assume(n>=1)
  x=1
  while (x<n)
    x=x+1;  
}

array.limit.cases = function(n) {
  params=list()
  for (i in 1:n)
    params=c(params,list(list(n=i)))
  return(params)
} 

list(array.limit, array.limit.cases(10))
