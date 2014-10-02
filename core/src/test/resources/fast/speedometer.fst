//! @tags fixpoint nonlinear
//! @citations GopanR07:f4:p10

model gopan_reps_cav06_10 {

	var d, t, s, c;
	states n1, n2, n3, n4, n5, n6;

	transition t1 := {
		from := n1;
		to := n2;
		guard := true;
		action := ;
	};

	transition t2 := {
		from := n2;
		to := n3;
		guard := true;
		action := t' = t + 1, s' = 0;
	};

	transition t3 := {
		from := n3;
		to := n6;
		guard := true;
		action := ;
	};

	transition t4 := {
		from := n1;
		to := n4;
		guard := true;
		action := ;
	};

	transition t5 := {
		from := n4;
		to := n5;
		guard := s < c;
		action := ;
	};

	transition t6 := {
		from := n5;
		to := n6;
		guard := true;
		action := d' = d + 1, s' = s + 1;
	};

	transition t7 := {
		from := n4;
		to := n6;
		guard := s >= c;
		action := ;
	};

	transition t8 := {
		from := n6;
		to := n1;
		guard := true;
		action := ;
	};

}

strategy s {

	Region init := {state = n1 && d = 0 && t = 0 && s = 0};

	Region bad := {state = n1 && (s > d || d > c * t + s || 0 > s || s > c)};

}

