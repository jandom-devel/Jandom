//! @tags fixpoint
//! @citations Gonnord_thesis:f4.9:p57

model gonnord_thesis_057 {

	var i, j;
	states q1, q2, q3;

	transition t1 := {
		from := q1;
		to := q2;
		guard := i <= 100;
		action := i' = i + 1;
	};

	transition t2 := {
		from := q2;
		to := q1;
		guard := true;
		action := ;
	};

	transition t3 := {
		from := q2;
		to := q2;
		guard := j <= 19;
		action := j' = i + j;
	};

	transition t4 := {
		from := q1;
		to := q3;
		guard := i > 100;
		action := ;
	};

}

strategy s {

	Region init := {state = q1 && i = 0 && j = -100};

	Region bad := {state = q1 && (0 > i || 0 > j + 100 || i > j + 201)};

}

