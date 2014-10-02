//! @citations HalbwachsPR97:f5:p9 Halbwachs_thesis Gonnord_thesis:p53:f4.6 Halbwachs93:f1:p3 Merchat_thesis
//! @tags fixpoint

model gonnord_thesis_053 {

	var d, v, t;
	states P, P_stop, P_wall, P_toofast;

	transition t1 := {
		from := P;
		to := P;
		guard := t <= 2;
		action := v' = 0, t' = t + 1;
	};

	transition t2 := {
		from := P;
		to := P;
		guard := v <= 1 && d <= 8;
		action := v' = v + 1, d' = d + 1;
	};

	transition t3 := {
		from := P;
		to := P_stop;
		guard := t >= 3;
		action := ;
	};

	transition t4 := {
		from := P;
		to := P_wall;
		guard := d >= 10;
		action := ;
	};

	transition t5 := {
		from := P;
		to := P_toofast;
		guard := v >= 3;
		action := ;
	};

}

strategy s {

	Region init := {state = P && d = 0 && v = 0 && t = 0};

	Region bad := {state = P_wall};

}

