floyd = function(C){
    A=c()
    P=c()
    i=1
    while (i<=3) {
        j=1
        while (j<=3)
        {
	    A[i + 3 * j-3] = C[i + 3 * j-3];
	    P[i + 3 * j-3] = -1;
	    j=j+1
        }
	i=i+1
    }
    i=1
    while (i<=3) {
        A[i + 3 * i-3] = 0;          #    /* no self cycle */
		i=i+1
    }
	k=1
    while (k<=3) { 
	i=1
        while (i<=3) {
	    j=1	
	    while (j<=3) {
		if (A[i + 3 * k-3]+A[k + 3 * j-3] < A[i + 3 * j-3])
		{
		    A[i + 3 * j-3] = A[i + 3 * k-3] + A[k + 3 * j-3];
		    P[i + 3 * j-3] = k;  # /* k is included in shortest path */
		}
		j=j+1
	     }
	     i=i+1
	}
        k=k+1
    }
   return(A)
}

list(floyd,list(C=c(0,1,4,100,0,2,100,100,0)))

