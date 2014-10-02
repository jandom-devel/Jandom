//! @citations BeyerHMR07:f4:p7 GulwaniZ10:f9:p12
//! @tags fixpoint

model beyer_henzinger_majumdar_rybalchenko_pldi07_07 {

	var x, y;
	states q1, q2, q3;

	transition t1 := {
		from := q1;
		to := q2;
		guard := x < 100;
		action := ;
	};

	transition t2 := {
		from := q2;
		to := q1;
		guard := x < 50;
		action := x' = x + 1;
	};

	transition t3 := {
		from := q2;
		to := q1;
		guard := x >= 50;
		action := x' = x + 1, y' = y + 1;
	};

	transition t4 := {
		from := q1;
		to := q3;
		guard := x >= 100;
		action := ;
	};

}

strategy s {

	Region init := {state = q1 && x = 0 && y = 50};

	//Region bad := {state = q3 && y > 100};

}

