# This is another example which shows benefits of back-edge widening. 
# Even in this case, back-edge widening may be replaced by delayed widening.
 
bakedge2 = function() {
  i = 0
  x = 0
  while ( TRUE ) {
    # we need i=10 and not i=1 because PPL widening has
    # intermediary thresholds for small numbers.
    if (x >= 100) 
      i = 10
    else
      i = -10    
    x = x + 1
  }
}
