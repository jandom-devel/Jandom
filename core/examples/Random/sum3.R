# This example is tailored for the domain Interval + Parallelotopes
# The wanted invariant inside the while loop is:
# [ 0 <= x <= 1, 0 <= y <= 1] + [ 0 <= x<= 10, x = y ]

function () {
  x = 0
  y = 0
  while (x < 10) {
    if (x <= y)  x = y else y = x
    x = x+1
    y = y+1
    if (brandom()) x = x-1 else y = y-1
  }
}
