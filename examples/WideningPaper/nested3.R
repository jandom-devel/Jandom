# This example is a simple nested loop. It has been devised to show
# benefits of the strategy of narrowing with restart. It is the only one that
# can prove that 1 <= i <= 10 inside the inner loop.

nested3 = function() {
  i = 0
  while (TRUE) {
     i = i + 1
     j = 0  
     while (j < 10) 
       j = j + 1   
    if (i > 9) i = 0
  } 
}
