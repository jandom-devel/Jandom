//! @tags complexity
//! @citations AliasDFG10

model determinant{

var i, j, k, n;

states start, 
       a, d, b, c, halt;

transition t0 :={
  from   := start;
  to     := a;
  guard  := n >= 1;
  action := ; 
};

transition t1 :={
  from   := a;
  to     := d;
  guard  := true;
  action := k' = 1;
};

transition t2 :={
  from   := d;
  to     := b;
  guard  := k < n;
  action := i' = k+1;
};

transition t3 :={
  from   := d;
  to     := halt;
  guard  := k >= n;
  action := i' = i;
};

transition t4 :={
  from   := b;
  to     := d;
  guard  := i > n;
  action := k' = k+1;
};

transition t5 :={
  from   := b;
  to     := c;
  guard  := i <= n;
  action := j' = n;
};

transition t6 :={
  from   := c;
  to     := c;
  guard  := j > k;
  action := j' = j-1;
};

transition t7 :={
  from   := c;
  to     := b;
  guard  := j <= k;
  action := i' = i+1;
};

}

strategy theStrat {
  Region init := {state = start};
}

