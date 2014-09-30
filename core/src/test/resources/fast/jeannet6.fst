//! @tags fixpoint
//! @citations Jeannet_thesis:p111:f8.5

model jeannet_thesis_111 {
	
	var x, y;
	states i, d;
	
	transition t1 := {
		from := i;
		to := i;
		guard := x <= 9;
		action := x' = x + 1;
	};
	
	transition t2 := {
		from := i;
		to := d;
		guard := x >= 10;
		action := x' = x + 1;
	};
	
	transition t3 := {
		from := d;
		to := d;
		guard := x >= 6;
		action := x' = x - 1;
	};
	
	transition t4 := {
		from := d;
		to := i;
		guard := x <= 5;
		action := x' = x - 1;
	};
	
}

strategy s {
	
	Region init := {state = i && x = 0};
	
	Region bad := {state = d && (5 > x || x > 11)};
	
}

