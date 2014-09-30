//! @tags fixpoint
//! @citations Gonnord_thesis:f2.15:p34

model gonnord_thesis_034 {

	var l, t, x;
	states L, N;

	transition t1 := {
		from := L;
		to := L;
		guard := x <= 9;
		action := x' = x + 1, t' = t + 1, l' = l + 1;
	};

	transition t2 := {
		from := L;
		to := N;
		guard := true;
		action := x' = 0;
	};

	transition t3 := {
		from := N;
		to := N;
		guard := true;
		action := x' = x + 1, t' = t + 1;
	};

	transition t4 := {
		from := N;
		to := L;
		guard := x >= 50;
		action := x' = 0;
	};

}

strategy s {

	Region init := {state = L && x = 0 && t = 0 && l = 0};

	Region bad := {6l > t + 50};

}

