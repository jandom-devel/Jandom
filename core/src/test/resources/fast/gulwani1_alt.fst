//! @tags complexity
//! @citations Gulwani09:f1b:p3

model gulwani_cav09_03b {

	var x, y, i1, i2, m;
	states q;

	transition t1 := {
		from := q;
		to := q;
		guard := x < 100 && y < m;
		action := y' = y + 1, i1' = i1 + 1;
	};

	transition t2 := {
		from := q;
		to := q;
		guard := x < 100 && y >= m;
		action := x' = x + 1, i2' = i2 + 1;
	};

}

strategy s {

	Region init := {state = q && x = 0 && y = 0 && m >= 0 && i1 = 0 && i2 = 0};

	Region bad := {i1 + i2 > 100 + m};

}

