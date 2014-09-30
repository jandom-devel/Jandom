//! @tags complexity
//! @citations GulwaniZ10:f3:p3

model gulwani_zuleger_pldi10_03b {

	var n, m, j;
	states q1, q2;

	transition t1a := {
		from := q1;
		to := q2;
		guard := j < n;
		action := ;
	};

	transition t1b := {
		from := q1;
		to := q2;
		guard := j > n;
		action := ;
	};

	transition t2 := {
		from := q2;
		to := q1;
		guard := j > m;
		action := j' = 0;
	};

	transition t3 := {
		from := q2;
		to := q1;
		guard := j <= m;
		action := j' = j + 1;
	};

}

strategy s {

	Region init := {state = q1 && 0 < n && n < m && j = n + 1};

}

