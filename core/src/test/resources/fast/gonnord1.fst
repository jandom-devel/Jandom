//! @tags fixpoint
//! @citations Gonnord_thesis:f4.3:p50

model gonnord_thesis_050 {

	var x, y;
	states p;

	transition t1 := {
		from := p;
		to := p;
		guard := x >= 0;
		action := x' = x + 1;
	};

	transition t2 := {
		from := p;
		to := p;
		guard := x = 0;
		action := x' = x + 1, y' = y + 1;
	};

}

strategy s {

	Region init := {state = p && x = 0 && y = 0};

	Region bad := {x < 0 || y < 0 || y > x || y > 1};

}

