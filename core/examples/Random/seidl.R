# This example has been discussed with Apinis, Seidl and Vojdani
# during the works on the integrating localized narrowing with
# the mixed widening/narrowing operator.

function() {
      i = 0
      while (i < 42) {
           j = 0
           while (j < 10)
                 j = j+1
           i = i+j
     } 
}

