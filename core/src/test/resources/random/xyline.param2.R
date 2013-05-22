## Versione parametrica di xyline più "buona"
##
## BOX invariante: nulla
## ROTATED BOX with GENERATORS new SIMPLIFY: -Inf<=-x+y<=Inf , 0<=x+y<=0
## COMBO BOX with GENERATORS new SIMPLIFY: -1<=x<=Inf , -Inf<=y<=1 : -Inf<=-x+y<=2 , 0<=x+y<=0 

xyline.param2=function(x) {
	assume(x>0)
	y=-x
	.tracetag(3)
	while(x>y) {
		x= x-1
		y= y+1
	}
}

xyline.param2.cases = function(n) {
	params=list()
	for (i in 1:n)
		params=c(params,list(list(x=i)))
	return(params)
}

list(xyline.param2, xyline.param2.cases(10))
