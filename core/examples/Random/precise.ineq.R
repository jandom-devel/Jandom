#
# Strict Inequalitities
#
# This example wants to show that inequalities and disequalities
# are handled very precisely in RANDOM.
#

#
# Target: at the end, x=4, y=0, z=0
# Intervals et al in RANDOM: OK!
# Intervals et octagon in INTERPROC: 0<=y<=1, 0<=z<=1

if (FALSE) "
/* INTEPROC version */
var x:real, y:real, z:real;
begin
  x=4;
  y=0;
  z=0;
  if (x<4) 
    y=1;
  endif;
  if (x!=4)
    z=1;
  endif
end
"

precise.ineq = function() {
  x = 4
  y = 0
  z = 0
  if (x<4) y=1
  if (x!=4) z=1
}
