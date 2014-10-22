// A model to test the sum domain

model sum {

     var i, x, y;

     states loop, firstif, secondif, endloop, end;
    
     transition exit := {
        from := loop;
        to := end;
        guard := i > 4;
        action := ;
     };
     
     transition incr := {
        from := loop;
        to := firstif;
        guard := i <= 4;
        action := i' = i + 1; 
     };
     
     transition firstif1 := {
        from := firstif;
        to := secondif;
        guard := TRUE;
        action := x' = i - 1;
     };
     
     transition firstif2 := {
        from := firstif;
        to := secondif;
        guard := TRUE;
        action := x' = i;
     };

     transition secondif1 := {
        from := secondif;
        to := endloop;
        guard := TRUE;
        action := y' = i - 1;
     };
     
     transition secondif2 := {
        from := secondif;
        to := endloop;
        guard := TRUE;
        action := y' = i;
     };
     
     transition again := {
        from := endloop;
        to := loop;
        guard := true;
        action := ;
     };
     
}

strategy s {
     Region init := { state = loop && i = 0 && x = 0 && y = 0 };
}
