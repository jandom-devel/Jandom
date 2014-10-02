//! @tags fixpoint
//! @citations Jeannet_thesis:p109:f8.4

model jeannet_thesis_109 {
	
	var x, y, e;
	states i, a, b;
	
	transition t1 := {
		from := i;
		to := a;
		guard := e = 1;
		action := ;
	};
	
	transition t2 := {
		from := i;
		to := b;
		guard := e = 0;
		action := ;
	};
	
	transition t3 := {
		from := a;
		to := a;
		guard := e = 1;
		action := x' = x + 1;
	};
	
	transition t4 := {
		from := a;
		to := a;
		guard := e = 1;
		action := x' = x + 1, y' = y + 1;
	};
	
	transition t5 := {
		from := a;
		to := b;
		guard := e = 0;
		action := ;
	};

	transition t6 := {
		from := b;
		to := a;
		guard := true;
		action := x' = x + 1, y' = y + 2;
	};

}

strategy s {
	
	Region init := {state = i && x = 0 && y = 0 && 0 <= e && e <= 1};
	
	Region bad := {state = b && y != 2x};
	
}

