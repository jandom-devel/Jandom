//! @citations KatzEWPS85
//! @tags parallelism

model katz_eggers_wood_perkins_sheldon_isca85 {

	var e, ne, uo, i;
	states k;

	transition t1 := {
		from := k;
		to := k;
		guard := i >= 1;
		action := ne' = ne + e, uo' = uo + 1, i' = i - 1, e' = 0;
	};

	transition t2 := {
		from := k;
		to := k;
		guard := ne + uo >= 1;
		action := i' = i + ne + uo - 1, e' = e + 1, ne' = 0, uo' = 0;
	};

	transition t3 := {
		from := k;
		to := k;
		guard := i >= 1;
		action := i' = i + e + ne + uo - 1, e' = 1, ne' = 0, uo' = 0;
	};

}

strategy s {

	Region init := {state = k && e = 0 && ne = 0 && uo = 0 && i >= 1};

	Region bad := {e = 1 && ne = 1};

}

