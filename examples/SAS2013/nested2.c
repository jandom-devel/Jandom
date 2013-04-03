# Nested2 in C for use with PAGAI. PAGAI is able to prove that i >= 0 
# in the inner loop, but not with standard AI technique, even with
# H&H widening.

int main() {
   int i = 0;
   while (1) {
     int j = 0;
     while (j < 10)
       j = j+1;
     i = i+11-j;
   }
}

