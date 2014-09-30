//! @tags fixpoint
//! @citations Gonnord_thesis:f7.7:p99

model gonnord_thesis_099 {

	var u, t, l, v;
	states q;

	transition t1 := {
		from := q;
		to := q;
		guard := u <= 59 && v <= 9;
		action := u' = u + 1, t' = t + 1, l' = l + 1, v' = v + 1;
	};

	transition t2 := {
		from := q;
		to := q;
		guard := u <= 59;
		action := u' = u + 1, t' = t + 1;
	};

	transition t3 := {
		from := q;
		to := q;
		guard := u = 60;
		action := u' = 0, v' = 0;
	};

}

strategy s {

	Region init := {state = q && u = 0 && t = 0 && l = 0 && v = 0};

	Region bad := {6l > t + 50};

}

