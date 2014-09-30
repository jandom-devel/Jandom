//! @tags complexity nonlinear
//! @citations GulwaniJK09:f4:p5

model gulwani_jain_koskinen_pldi09_05a {

	var v1, v2, n, m, i;
	states q;

	transition t1 := {
		from := q;
		to := q;
		guard := v1 > 0 && v2 < m;
		action := v2' = v2 + 1, v1' = v1 - 1, i' = i + 1;
	};

	transition t2 := {
		from := q;
		to := q;
		guard := v1 > 0 && v2 >= m;
		action := v2' = 0, i' = i + 1;
	};

}

strategy s {

	Region init := {state = q && n > 0 && m > 0 && v1 = n && v2 = 0};

	Region bad := {m * i > n + m * n};

}

