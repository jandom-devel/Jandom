//! @tags complexity
//! @citations GulwaniMC09:f4:p6

model gulwani_mehra_chilimbi_popl09_06e {

	var x, y, n, m, c1, c2;
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
		guard := y < m;
		action := y' = y + 1, c1' = c1 + 1;
	};

	transition t3 := {
		from := q2;
		to := q1;
		guard := y >= m;
		action := x' = x + 1, c2' = c2 + 1;
	};

}

strategy s {

	Region init := {state = q1 && x = 0 && y = 0 && c1 = 0 && c2 = 0 && n >= 0 && m >= 0};

	Region bad := {c1 + c2 > n + m};

}

