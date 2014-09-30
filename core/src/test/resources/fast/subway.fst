//! @tags fixpoint
//! @citations Halbwachs93:f9:p12 HalbwachsPR97:f8:p15

model halbwachs_cav93_9 {

	var s, b, d;
	states T, L, B, S;

	transition t1 := {
		from := T;
		to := T;
		guard := b - s < 9;
		action := b' = b + 1;
	};

	transition t2 := {
		from := T;
		to := T;
		guard := b - s > -9;
		action := s' = s + 1;
	};

	transition t3 := {
		from := T;
		to := L;
		guard := b - s = -9;
		action := s' = s + 1;
	};

	transition t4 := {
		from := L;
		to := T;
		guard := b - s = -1;
		action := b' = b + 1;
	};

	transition t5 := {
		from := L;
		to := L;
		guard := b - s < -1;
		action := b' = b + 1;
	};

	transition t6 := {
		from := T;
		to := B;
		guard := b - s = 9;
		action := b' = b + 1, d' = 0;
	};

	transition t7 := {
		from := B;
		to := T;
		guard := b - s = 1;
		action := s' = s + 1;
	};

	transition t8 := {
		from := B;
		to := B;
		guard := d < 9;
		action := b' = b + 1, d' = d + 1;
	};

	transition t9 := {
		from := B;
		to := B;
		guard := b - s > 1;
		action := s' = s + 1;
	};

	transition t10 := {
		from := B;
		to := S;
		guard := d <= 9;
		action := b' = b + 1;
	};

	transition t11 := {
		from := S;
		to := S;
		guard := b - s > 1;
		action := s' = s + 1;
	};

	transition t12 := {
		from := S;
		to := T;
		guard := b - s = 1;
		action := s' = s + 1;
	};

}

strategy s {

	Region init := {state = T && s = 0 && b = 0 && d = 0};

	Region bad := {-10 > b - s || b - s > 20};

}

