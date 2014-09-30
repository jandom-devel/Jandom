//! @tags complexity
//! @citations GulwaniMC09:f4:p6

model gulwani_mehra_chilimbi_popl09_06f {

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
		to := q2;
		guard := y < m;
		action := y' = y + 1, c1' = c1 + 1;
	};

	transition t3 := {
		from := q2;
		to := q1;
		guard := true;
		action := x' = x + 1, c2' = c2 + 1;
	};

}

strategy s {

	Region init := {state = q1 && x = x0 && y = y0 && 0 <= x0 && x0 <= n && 0 <= y0 && y0 <= m && c1 = 0 && c2 = 0 && n >= 0 && m >= 0};

	Region bad := {c1 + c2 > n + m - x0 - y0};

}

