using System;
using System.Diagnostics.Contracts;

[ContractVerification(true)]
public class Program
{ 
  public static int Puzzle() {
    int i,j;
    i=0;
    j=0;
    while (true){
      i=i+1;
      j=0;
      while (j<10) {
        Contract.Assert(i <= 10);
        j = j + 1;        
      }
      if (i>9) i=0;
    }
    return i;
  }
}

