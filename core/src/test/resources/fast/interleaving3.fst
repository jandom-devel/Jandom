//! @tags complexity nonlinear
//! @citations GulwaniJK09:f4:p5

model gulwani_jain_koskinen_pldi09_05c {

	var i, m, n, c;
	states q;

	transition t1 := {
		from := q;
		to := q;
		guard := i > 0 && i < m;
		action := i' = i - 1, c' = c + 1;
	};

	transition t2 := {
		from := q;
		to := q;
		guard := i > 0 && i >= m;
		action := i' = i - m, c' = c + 1;
	};

}

strategy s {

	Region init := {state = q && 0 < m && m < n && i = n};

	Region bad := {m * i > n + m * m};

}

