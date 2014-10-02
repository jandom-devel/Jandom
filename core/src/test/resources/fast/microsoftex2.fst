//! @tags complexity
//! @citations GulwaniZ10:f2:p4

model gulwani_zuleger_pldi10_04a {

	var n, n0, m, i;
	states q1, q2;

	transition t1 := {
		from := q1;
		to := q2;
		guard := n > 0 && m > 0;
		action := n' = n - 1, m' = m - 1, i' = i + 1;
	};

	transition t2 := {
		from := q2;
		to := q2;
		guard := true;
		action := n' = n - 1, m' = m + 1;
	};

	transition t3 := {
		from := q2;
		to := q1;
		guard := true;
		action := ;
	};

}

strategy s {

	Region init := {state = q1 && n > 0 && m > 0 && n = n0 && i = 0};

	Region bad := {i > n0};

}

