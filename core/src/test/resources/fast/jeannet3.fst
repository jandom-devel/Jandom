//! @tags fixpoint
//! @citations Jeannet_thesis:f4.2:p41

model jeannet_thesis_041 {
	
	var x, y;
	states k1, k2, k3, k4, err;
	
	transition t1 := {
		from := k1;
		to := k2;
		guard := x >= y;
		action := x' = x + 1;
	};
	
	transition t2 := {
		from := k2;
		to := k3;
		guard := x >= y;
		action := y' = y + 1;
	};
	
	transition t3 := {
		from := k3;
		to := k4;
		guard := x >= y;
		action := x' = x + 1;
	};
	
	transition t4 := {
		from := k4;
		to := k1;
		guard := x >= y;
		action := y' = y + 1;
	};
	
	transition t5 := {
		from := k1;
		to := err;
		guard := x < y;
		action := ;
	};

	transition t6 := {
		from := k2;
		to := err;
		guard := x < y;
		action := ;
	};

	transition t7 := {
		from := k3;
		to := err;
		guard := x < y;
		action := ;
	};

	transition t8 := {
		from := k4;
		to := err;
		guard := x < y;
		action := ;
	};

}

strategy s {
	
	Region init := {state = k1 && x = 0 && y = 0};
	
	Region bad := {state = err};
	
}

