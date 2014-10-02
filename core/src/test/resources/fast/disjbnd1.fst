//! @tags complexity
//! @citations GulwaniMC09:f2:p3

model gulwani_mehra_chilimbi_popl09_03a {

	var x0, y0, x, y, n, m, c1, c2;
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

	Region init := {state = q1 && c1 = 0 && c2 = 0 && x = x0 && y = y0 && n >= x0 && m >= y0};

	Region bad := {c1 + c2 > n - x0 + m - y0};

}

