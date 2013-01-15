#
# This shows the benefits of Input Scope Widening. When scope is Input, and without narowing steps,
# the result is -1 <= i <= 1. Otherwise, the result is -\infty <= i <= +\infty
#
localwidening = function() {
  i = 0
  while (TRUE) {
    tag(0)
    if (brandom()) 
      i = 1
    else
      i = -1
  }
}
