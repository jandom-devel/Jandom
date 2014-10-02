//! @tags complexity nonlinear
//! @citations Gulwani09:f2b:p4

model gulwani_cav09_04b {

	var x, y, m, n, i1, i2;
	states q;

	transition t1 := {
		from := q;
		to := q;
		guard := x < n && y < m;
		action := y' = y + 1, i1' = i1 + 1;
	};

	transition t2 := {
		from := q;
		to := q;
		guard := x < n && y >= m;
		action := y' = 0, x' = x + 1, i2' = i2 + 1;
	};

}

strategy s {

	Region init := {state = q && x = 0 && y = 0 && m >= 0 && n >= 0 && i1 = 0 && i2 = 0};

	Region bad := {i1 + i2 > n * (m + 1)};

}

