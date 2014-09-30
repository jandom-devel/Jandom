//! @tags complexity
//! @citations GulwaniZ10:f2:p4

model gulwani_zuleger_pldi10_04b {

	var flag, n, n0, i;
	states q1, q2;

	transition t1 := {
		from := q1;
		to := q2;
		guard := flag = 1;
		action := flag' = 0, i' = i + 1;
	};

	transition t2 := {
		from := q2;
		to := q2;
		guard := n > 0;
		action := n' = n - 1, flag' = 1;
	};

	transition t3 := {
		from := q2;
		to := q1;
		guard := true;
		action := ;
	};

}

strategy s {

	Region init := {state = q1 && n >= 0 && n = n0 && flag = 1 && i = 0};

	Region bad := {i > n0 + 1};

}

