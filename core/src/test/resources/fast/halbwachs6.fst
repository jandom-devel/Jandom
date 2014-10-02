//! @tags fixpoint
//! @citations Halbwachs_misc10:p40

model halbwachs_aussois10_40 {

	var v, t, x, d;
	states k;

	transition t1 := {
		from := k;
		to := k;
		guard := x <= 4;
		action := x' = x + 1, v' = v + 1;
	};

	transition t2 := {
		from := k;
		to := k;
		guard := d <= 9;
		action := d' = d + 1, t' = t + 1;
	};

	transition t3 := {
		from := k;
		to := k;
		guard := d = 10 && x >= 2;
		action := x' = 0, d' = 0;
	};
	
}

strategy s {

	Region init := {state = k && v = 0 && t = 0 && x = 0 && d = 0};

	Region bad := {v < 0 || t < 0 || 2v > t + 10 || 5v < t - 10};

}

