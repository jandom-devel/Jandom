//! @tags fixpoint
//! @citations Halbwachs_misc10:p08

model halbwachs_aussois10_08 {

	var x, y;
	states k;

	transition t1 := {
		from := k;
		to := k;
		guard := x <= 100;
		action := x' = x + 2;
	};

	transition t2 := {
		from := k;
		to := k;
		guard := x <= 100;
		action := x' = x + 1, y' = y + 1;
	};

}

strategy s {

	Region init := {state = k && x = 0 && y = 0};

	Region bad := {y > x || y < 0 || x < 0};

}

