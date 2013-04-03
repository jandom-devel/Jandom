# This example is a simple nested loop. It has been devised to show
# benefits of localized narrowing. Both strategies are able to prove that 
# i >= 0, while standard narrowing cannot prove it, even with localized
# widening.

function() {
  i = 0
  while (TRUE) {
     j = 0  
     while (j < 10) 
       j = j + 1
     i =  i + 11 - j
  } 
}
