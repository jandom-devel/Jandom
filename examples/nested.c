# Nested in C for use with PAGAI. PAGAI is able to prove that i <= 10 
# using standard AI technique with new narrowing.

int main() {
   int i = 0;
   while (i < 10) {
     int j = 0;
     while (j < 10)
       j = j+1;
     i = i+1;
   }
}

