###
### Parallels: two exponentials computed in parallel 
###

## 
## Both PCA and OSCA cannot find any result
## Using the matrix: 
## 1 -1
## 1 -2
## we can prove the invariant x=y.
## The matrix can be found as follows (maybe): 
## - find the first principal component of the PCA
## - remove all points which have variance zero, and find again 
##   the first principal component of the PCA

## ICA with theta=0.80 singular
## ICA with theta=0.98 OK!

parallel.lines = function() {
	x=1
	y=1
	while (TRUE) {
		x = x+y
		y = 2*y
	}	
}
