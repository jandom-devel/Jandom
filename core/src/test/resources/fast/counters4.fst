//! @tags complexity
//! @citations GulwaniMC09:f3:p6

model gulwani_mehra_chilimbi_popl09_06d {

	var x, y, n, m, c;
	states q;

	transition t1 := {
		from := q;
		to := q;
		guard := x < n;
		action := x' = x + 1, y' = y + 1, c' = c + 1;
	};

	transition t2 := {
		from := q;
		to := q;
		guard := x >= n && y < m;
		action := x' = x + 1, y' = y + 1, c' = c + 1;
	};

}

strategy s {

	Region init := {state = q && x = 0 && y = 0 && c = 0 && n >= 0 && m >= 0};

	Region bad := {c > n && c > m};

}

