## Versione parametrica di xyline
##
## BOX:
##   invariante: 0<=k
##
## ROTATED BOX, COMBINED BOX: come BOX
##   le tre componenti principali sono "k, x+y, x-y" e quindi non si ha relazione tra k ed x, y
##
## ROTATED BOX with GENERATOR (10):
##   invariante: x-k<=0, 0<=k+x+2*y
## COMBINED BOX with GENERATOR (10):
##   invariante: come ROTATED BOX + BOX, in più -k-x+y<=2
##
## inverted ROTATED BOX with GENERATOR (10): nulla
## inverted COMBINED BOX with GENERATOR (10): come BOX
##
## ROTATED BOX AD HOC:
##   analyze.function(xyline.param,rotatedbox.domain(solve(matrix(c(1,0,-1,1,-1,0,1,1,0),nrow=3, byrow=TRUE))), vars=c("x","y","k"),displayed.forms=c("x","y","k","x-y","x+y","x-k","k+x+2*y","-k-x+y"))
##   invariante: -1<=x, y<=1, -1<=k, -2<=x-y, 0<=x+y<=0, x-k<=0  (0<=k+x+2*y, -k-x+y<=3)
## ----- COMBINED BOX AD HOC:
##   stessa matrice di cui sopra
##   invariante come sopra, in più: 0<=k (-k-x+y<=2)
##
## INTERPROC
##   box: stessi risultati dei nostri box
##   octagon: ottengo gli stessi risultati di Ad-Hoc Combined Box. Ma il nostro tool consente anche di ottenere 
##            proprietà derivate come 0<=k+x+2*y
##

xyline.param=function(k) {
  assume(k>0)
  x=k
  y=-k
  while(x>y) {
    x= x-1
    y= y+1
  }
}

xyline.param.cases = function(n) {
  params=list()
  for (i in 1:n)
    params=c(params,list(list(k=i)))
  return(params)
}

list(xyline.param, xyline.param.cases(10))
