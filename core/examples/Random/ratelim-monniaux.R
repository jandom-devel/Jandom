ratelim_monniaux = function() {
  x_old = 0
  while (TRUE) {
    assume(x>= -100000 && x <=100000)
    if (x > x_old+10) x= x_old +10
    if (x < x_old-10) x= x_old -10
    x_old = x
  }
}