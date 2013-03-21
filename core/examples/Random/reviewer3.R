#
# Program suggested by reviewer 3 of JSC
#

# Target: at the beggining of while: y <= 2x, y >= -2x
# PCA and ICA find the standard axes
# Ad-Hoc matrices work

reviewer3 <- function () {
  x = 0
  y = 0
  while ( TRUE ) {
    y = y + 1;
    if ( brandom() ) 
	x = 2 * y
    else 
	x = - 2 * y
  }
}


