#
# This shows the benefits of widening at back edges. When scope is BackEdge
# the result is 0 <= i <= 10. Otherwise, the result is 0 <= i <= +\infty.
# The same result may be obtained with a delayed widening.
#
# Note that if we use i=1 it may not work, due to some optimizations of the
# PPL widening.

backedge = function() {
  i=0
  while (TRUE) {
    if (brandom()) 
      i = 10
  }
}
