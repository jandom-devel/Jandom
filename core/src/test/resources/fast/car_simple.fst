//! @citations HalbwachsPR97 Gonnord_thesis:p96:f7.5
//! @tags fixpoint

model gonnord_thesis_096a {

	var s, t, d;
	states q;

	transition t1 := {
		from := q;
		to := q;
		guard := s <= 3;
		action := d' = d + 1, s' = s + 1;
	};

	transition t2 := {
		from := q;
		to := q;
		guard := true;
		action := s' = 0, t' = t + 1;
	};

}

strategy s {

	Region init := {state = q && s = 0 && t = 0 && d = 0};

	Region bad := {t < 0 || 0 > s || s > 4 || 0 > d || d > 4t + s};

}

