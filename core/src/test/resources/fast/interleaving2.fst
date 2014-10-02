//! @tags complexity nonlinear
//! @citations GulwaniJK09:f4:p5

model gulwani_jain_koskinen_pldi09_05b {

	var i, j, m, n, c;
	states q;

	transition t1 := {
		from := q;
		to := q;
		guard := i < n && j < m;
		action := j' = j + 1, c' = c + 1;
	};

	transition t2 := {
		from := q;
		to := q;
		guard := i < n && j >= m;
		action := j' = 0, i' = i + 1, c' = c + 1;
	};

}

strategy s {

	Region init := {state = q && 0 < m && m < n && i = 0 && j = 0 && c = 0};

	Region bad := {c > n * m};

}

