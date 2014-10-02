//! @tags fixpoint
//! @citations Halbwachs_misc10:p43

model halbwachs_aussois10_43 {

	var t, s, d;
	states k;

	transition t1 := {
		from := k;
		to := k;
		guard := s <= 3;
		action := s' = s + 1, d' = d + 1;
	};

	transition t2 := {
		from := k;
		to := k;
		guard := true;
		action := t' = t + 1, s' = 0;
	};

}

strategy s {

	Region init := {state = k && t = 0 && s = 0 && d = 0};

	Region bad := {t < 0 || 0 > s || s > 4 || 0 > d || d > 4t + s};

}

