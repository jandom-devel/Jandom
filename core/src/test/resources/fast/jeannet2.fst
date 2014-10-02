//! @tags fixpoint
//! @citations Jeannet_thesis:f4.1:p40

model jeannet_thesis_040 {
	
	var x, y;
	states k1, k2, err;
	
	transition t1 := {
		from := k1;
		to := k1;
		guard := x < 9;
		action := x' = x + 1;
	};
	
	transition t2 := {
		from := k1;
		to := k2;
		guard := x = 9;
		action := x' = x + 1;
	};
	
	transition t3 := {
		from := k2;
		to := k2;
		guard := true;
		action := x' = x + 1, y' = y + 1;
	};
	
	transition t4 := {
		from := k2;
		to := err;
		guard := x <= 13 && y >= 4;
		action := ;
	};
	
}

strategy s {
	
	Region init := {state = k1 && x = 0 && y = 0};
	
	Region bad := {state = err};
	
}

