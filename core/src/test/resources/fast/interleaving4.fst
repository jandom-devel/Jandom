//! @tags complexity
//! @citations GulwaniJK09:f4:p5

model gulwani_jain_koskinen_pldi09_05d {

	var i, m, n, b, c;
	states q;

	transition t1 := {
		from := q;
		to := q;
		guard := 0 < i && i < n && b = 1;
		action := i' = i + 1, c' = c + 1;
	};

	transition t2 := {
		from := q;
		to := q;
		guard := 0 < i && i < n && b = 0;
		action := i' = i - 1, c' = c + 1;
	};

}

strategy s {

	Region init := {state = q && 0 < m && m < n && i = m && c = 0};

	Region bad := {c > m && c > n - m};

}

