# This example is a simple nested loop. It has been devised to show
# benefits of the strategy of narrowing with restart or continue. Both
# startegies are able to prove that i >= 0, while Separate strategy cannot
# even with Random or Back-Edge widening.

nested2 = function() {
  i = 0
  while (TRUE) {
     j = 0  
     while (j < 10)
       j = j + 1
     i =  i + 11 - j
  } 
}
