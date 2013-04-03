# Nested3 in C for use with PAGAI. PAGAI is able to prove that 1 <= i <= 10 in
# the inner loop when using standard AI techniques with H&H narrowing.

int main() {
   int i = 0;
   while (1) {
     i = i+1;
     int j = 0;
     while (j < 10)
       j = j+1;
     if (i > 9) i=0;	
   }
}

