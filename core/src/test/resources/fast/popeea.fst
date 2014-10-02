//! @tags fixpoint
//! @citations PopeeaC06:p2 GulwaniZ10:f9:p12

model popeea_chin_asian06_02 {

	var x, l, upd, N;
	states q1, q2, q3, q4;

	transition t1 := {
		from := q1;
		to := q2;
		guard := x < N;
		action := ;
	};

	transition t2 := {
		from := q2;
		to := q3;
		guard := true;
		action := l' = x, upd' = 1;
	};

	transition t3 := {
		from := q2;
		to := q3;
		guard := true;
		action := ;
	};

	transition t4 := {
		from := q3;
		to := q1;
		guard := true;
		action := x' = x + 1;
	};

	transition t5 := {
		from := q1;
		to := q4;
		guard := x >= N;
		action := ;
	};

}

strategy s {

	Region init := {state = q1 && x = 0 && upd = 0};

	Region bad := {state = q4 && upd = 1 && (0 > l || l >= N)};

}

