//! @tags fixpoint
//! @citations Gonnord_thesis:f6.8:p83 Halbwachs_thesis

model gonnord_thesis_083 {

	var x, y;
	states q1, q2;

	transition t1 := {
		from := q1;
		to := q1;
		guard := x <= 100;
		action := x' = x + 1, y' = y + 1;
	};

	transition t2 := {
		from := q1;
		to := q1;
		guard := x <= 100;
		action := x' = x + 2;
	};

	transition t3 := {
		from := q1;
		to := q2;
		guard := x > 100;
		action := ;
	};

}

strategy s {

	Region init := {state = q1 && x = 0 && y = 0};

	Region bad := {y > x || y > -x + 202 || x > 102 || y < 0};

}

