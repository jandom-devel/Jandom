//! @tags complexity nonlinear
//! @citations Gulwani09:f2a:p4

model gulwani_cav09_04a {

	var x, y, m, n, i;
	states q;

	transition t1 := {
		from := q;
		to := q;
		guard := x < n && y < m;
		action := y' = y + 1, i' = i + 1;
	};

	transition t2 := {
		from := q;
		to := q;
		guard := x < n && y >= m;
		action := y' = 0, x' = x + 1, i' = i + 1;
	};

}

strategy s {

	Region init := {state = q && x = 0 && y = 0 && m >= 0 && n >= 0 && i = 0};

	Region bad := {i > n * (m + 1)};

}

