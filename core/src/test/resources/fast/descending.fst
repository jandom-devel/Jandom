// A model generating a very long descending chain
// in the absence of narrowing.

model narrowing {

     var n;

     states loop1, loop2;
 
     transition jump1 := {
        from := loop1;
        to := loop1;
        guard := n <= 10;
        action := n' = n+1;
     };
     
     transition changeloop := {
        from := loop1;
        to := loop2;
        guard := n >= 11;
        action := ;
     };
     
     transition jump2 := {
        from := loop2;
        to := loop2;
        guard := n <= 100;
        action := n' = n-1;
     };
}

strategy s {
     Region init := { state = loop1 && n = 0 };
}
