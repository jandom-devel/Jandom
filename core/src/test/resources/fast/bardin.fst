//! @citations Bardin_thesis:p41:f2.3 Gonnord_thesis:p42:f3.2
//! @tags fixpoint

model bardin_thesis_041 {

	var x, y;
	states q1, q2;

	transition t1 := {
		from := q1;
		to := q1;
		guard := x >= 0;
		action := x' = x + 2;
	};

	transition t2 := {
		from := q2;
		to := q2;
		guard := true;
		action := x' = x + 1, y' = y + 1;
	};

	transition t3 := {
		from := q1;
		to := q2;
		guard := x >= y;
		action := ;
	};

	transition t4 := {
		from := q2;
		to := q1;
		guard := true;
		action := x' = x - y;
	};

}

strategy s {

	Region init := {state = q1 && x >= 0};
	
	Region bad := {state = q1 && x < 0};

}

