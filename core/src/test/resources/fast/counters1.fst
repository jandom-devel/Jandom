//! @tags complexity
//! @citations GulwaniMC09:f3:p6

model gulwani_mehra_chilimbi_popl09_06a {

	var x, n, c;
	states q1, q2;

	transition t1 := {
		from := q1;
		to := q2;
		guard := x < n;
		action := ;
	};

	transition t2 := {
		from := q2;
		to := q1;
		guard := true;
		action := x' = x + 1, c' = c + 1;
	};

	transition t3 := {
		from := q2;
		to := q1;
		guard := true;
		action := x' = x + 1, c' = c + 1;
	};

}

strategy s {

	Region init := {state = q1 && x = 0 && c = 0 && n >= 0};

	Region bad := {c > n};

}

