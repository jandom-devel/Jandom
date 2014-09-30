//! @citations BeyerHMR07:f1:p3
//! @tags fixpoint

model beyer_henzinger_majumdar_rybalchenko_pldi07_03 {

	var n, i, a, b;
	states q1, q2, q3, q4;

	transition t1 := {
		from := q1;
		to := q2;
		guard := i < n;
		action := ;
	};

	transition t2 := {
		from := q2;
		to := q3;
		guard := true;
		action := a ' = a + 1, b' = b + 2;
	};

	transition t3 := {
		from := q2;
		to := q3;
		guard := true;
		action := a ' = a + 2, b' = b + 1;
	};

	transition t4 := {
		from := q3;
		to := q1;
		guard := true;
		action := i' = i + 1;
	};

	transition t5 := {
		from := q1;
		to := q4;
		guard := i >= n;
		action := ;
	};

}

strategy s {

	Region init := {state = q1 && n >= 0 && i = 0 && a = 0 && b = 0};

	Region bad := {state = q4 && a + b != 3n};

}

