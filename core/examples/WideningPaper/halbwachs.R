# Example 3 from Halbawachs: when the decreasing sequence fails. 

# We are able to prove, like Pagai, "i <= j+3" is an invariant in the
# inner while, both with Random widening, or with Restart narrowing.
# It does not work with Continue narrowing.

halbawach2 = function() {
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
