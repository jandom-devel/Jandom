using System;
using System.Diagnostics.Contracts;

[ContractVerification(true)]
public class Program
{ 
  public static int Puzzle() {
    Contract.Ensures(Contract.Result<int>() == 10);
    int i,j;
    i=0;
    j=0;
    while (true){
      Contract.Assert(i >= 0);
      j=0;
      while (j<10)
        j = j + 1;        
      i = i + 11 - j;      
    }
    return i;
  }
}

