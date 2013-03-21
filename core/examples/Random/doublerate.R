##
## Doublerate: a variant of incr with two counters, one
## incremented by 1, the other by 2. The invariant 2i-j=0
## holds, but cannot be expressed in Octagon.
##

## BOX
##   1^ istruzione while: [0<=i<=100] , [0<=j<=Inf] , 0<=i+j<=Inf , -100<=-i+j<=Inf , 0<=i+2*j<=Inf , -200<=-2*i+j<=Inf 
## PTOPES THETA=0.80
##   1^ istruzione while: -Inf<=i<=Inf , 0<=j<=Inf , [0<=i+j<=Inf] , [0<=-i+j<=Inf] , 0<=i+2*j<=Inf , -Inf<=-2*i+j<=Inf 
## PTOPES THETA=0.98
##   1^ istruzione while:   0<=i<=100,   0<=j<=200 ,  0<=i+j<=300,    0<=-i+j<=100 , [0<=i+2*j<=500] , [0<=-2*i+j<=0]
## COMBO THETA = 0.98 (come PTOPES)  
##   1^ istruzione while: [0<=i<=100], [0<=j<=200] , 0<=i+j<=300 , 0<=-i+j<=100 , [0<=i+2*j<=500] , [0<=-2*i+j<=0]
## ICA come PCS

doublerate = function() 
{
   i = 0
   j = 0   
   while (i <= 100) {
     i = i+1
     j = j+2
   }
}
