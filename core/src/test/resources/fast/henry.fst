//! @tags fixpoint
//! @citations Henry_misc11:p19

model henry_gdrgpl_19 {

	var x, d;
	states k;

	transition t1 := {
		from := k;
		to := k;
		guard := x = 0;
		action := d' = 1, x' = x + d;
	};

	transition t2 := {
		from := k;
		to := k;
		guard := 1 <= x && x <= 999;
		action := x' = x + d;
	};

	transition t3 := {
		from := k;
		to := k;
		guard := x = 1000;
		action := d' = -1, x' = x + d;
	};

}

strategy s {

	Region init := {state = k && x = 0 && d = 1};

	Region bad := {d > 1 || d < -1 || x < 0 || d < 2x - 1999};

}

