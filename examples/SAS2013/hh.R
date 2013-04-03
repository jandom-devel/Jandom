# Example 3 from Halbwachs & Henry: When the decreasing sequence fails. 

# We are able to prove, like Pagai, "i <= j+3" is an invariant in the
# inner while, both with Localized Widening, or with Hybrid Localized 
# Narrowing. It does not work with Continue Localized Narrowing.

function() {
  i = 0
  while (i < 4)  { 
    j = 0 
    while (j < 4)  {
      i = i+1 
      j = j + 1
    } 
    i =  i - j +1
  }
}
