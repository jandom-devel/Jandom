# This example is tailored for the domain Interval + Parallelotopes
# The wanted invariant at the last line of the while loop is:
# [ -1 <= x <= 0, -1 <= y <= 0, i = 0 ] + [ 1 <= i <= 10, x = i, y =i ]

function () {
  i = 0
  x = 0
  y = 0
  while (i < 10) {
    i = i+1
    if (brandom()) x = i-1 else x = i
    if (brandom()) y = i-1 else y = i
 }
}

