# This example is a simple nested loop. It has been devised to show
# benefits of localized narrowing with restart/hybrid strategies.  
# They are the only ones which may prove 1 <= i <= 10 inside the inner loop.

function() {
  i = 0
  while (TRUE) {
     i = i + 1
     j = 0  
     while (j < 10) 
       j = j + 1   
    if (i > 9) i = 0
  } 
}
