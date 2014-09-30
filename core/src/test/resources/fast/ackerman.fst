//! @citations BenAmramL07
//! @tags complexity

model ack {
  var m, n;
  states start, a,b, cont1,stop3,stop1,stop2;

  transition t0 := {
    from   := start;
    to     := stop1;
    guard  := m <= 0;
    action := ;
  };
 
  transition t0bis := {
    from   := start;
    to     := cont1;
    guard  := m >0;
    action := ;
  };
 
  transition t1 := {
    from   := cont1;
    to     := stop2;
    guard  := n <= 0;
    action := m' = m-1, n' = 1;
  };

  transition t2 := {
    from   := cont1;
    to     := a;
    guard  := n>0;
    action := n' = n-1;
  };

  transition t3 := {
    from   := a;
    to     := b;
    guard  := true;
    action := m' = m-1, n' = ?;
  };

  transition t4 := {
    from   := b;
    to     := start;
    guard  := n >= 0;
    action := ;
  };

  transition t4bis := {
    from   := b;
    to     := stop3;
    guard  := n < 0;
    action := ;
  };

}

strategy dumb {
  Region init := {state = start && n>=0 && m >= 0};

}

