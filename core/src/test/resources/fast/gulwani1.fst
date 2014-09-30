//! @tags complexity
//! @citations Gulwani09:f1a:p3

model gulwani_cav09_03a {

	var x, y, i, m;
	states q;

	transition t1 := {
		from := q;
		to := q;
		guard := x < 100 && y < m;
		action := y' = y + 1, i' = i + 1;
	};

	transition t2 := {
		from := q;
		to := q;
		guard := x < 100 && y >= m;
		action := x' = x + 1, i' = i + 1;
	};

}

strategy s {

	Region init := {state = q && x = 0 && y = 0 && m >= 0 && i = 0};

	Region bad := {i > 100 + m};

}

