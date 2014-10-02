//! @tags complexity
//! @citations GulwaniZ10:f2:p4

model gulwani_zuleger_pldi10_04c {

	var flag, n, n0, i, c;
	states q1, q2;

	transition t1 := {
		from := q1;
		to := q2;
		guard := i < n;
		action := flag' = 0, c' = c + 1;
	};

	transition t2 := {
		from := q2;
		to := q2;
		guard := true;
		action := flag' = 1, n' = n - 1;
	};

	transition t3 := {
		from := q2;
		to := q1;
		guard := flag = 1;
		action := ;
	};

	transition t4 := {
		from := q2;
		to := q1;
		guard := flag = 0;
		action := i' = i + 1;
	};

}

strategy s {

	Region init := {state = q1 && n >= 0 && n = n0 && flag = 1 && i = 0 && c = 0};

	Region bad := {c > n0};

}

