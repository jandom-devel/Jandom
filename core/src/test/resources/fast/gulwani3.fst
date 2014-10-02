//! @tags complexity
//! @citations Gulwani09:f3:p6

model gulwani_cav09_06 {

	var x, y, n, i1, i2;
	states q1, q2;

	transition t1 := {
		from := q1;
		to := q2;
		guard := x < n;
		action := i1' = i1 + 1, y' = x;
	};

	transition t2 := {
		from := q2;
		to := q2;
		guard := y < n;
		action := i2' = i2 + 1, y' = y + 1;
	};

	transition t3 := {
		from := q2;
		to := q1;
		guard := true;
		action := x' = y + 1;
	};

}

strategy s {

	Region init := {state = q1 && n >= 0 && x = 0 && i1 = 0 && i2 = 0};

	Region bad := {i2 > n};

}

