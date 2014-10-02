//! @tags fixpoint
//! @citations Halbwachs_misc10:p16

model halbwachs_aussois10_16 {

	var b, ok, x, y;
	states k;

	transition t1 := {
		from := k;
		to := k;
		guard := b = 0 && ok = 1 && x >= y;
		action := b' = 1, x' = x + 1, ok' = 1;
	};

	transition t2 := {
		from := k;
		to := k;
		guard := b = 0 && ok = 0;
		action := b' = 1, x' = x + 1, ok' = 0;
	};

	transition t3 := {
		from := k;
		to := k;
		guard := b = 0 && x < y;
		action := b' = 1, x' = x + 1, ok' = 0;
	};

	transition t4 := {
		from := k;
		to := k;
		guard := b = 1 && ok = 1 && x >= y;
		action := b' = 0, y' = y + 1, ok' = 1;
	};

	transition t5 := {
		from := k;
		to := k;
		guard := b = 1 && ok = 0;
		action := b' = 0, y' = y + 1, ok' = 0;
	};

	transition t6 := {
		from := k;
		to := k;
		guard := b = 1 && x < y;
		action := b' = 0, y' = y + 1, ok' = 0;
	};

}

strategy s {

	Region init := {state = k && b = 0 && ok = 1 && x = 0 && y = 0};
	
	Region bad := {ok = 0};

}

