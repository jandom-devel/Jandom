//! @citations BultanGP97:f3:p8
//! @tags fixpoint parallelism

model bultan_gerber_pugh_cav97_08 {

	var a, b, t, s, pc1, pc2;
	states q;

	transition e_T1 := {
		from := q;
		to := q;
		guard := pc1 = 1;
		action := pc1' = 2, a' = t, t' = t + 1;
	};

	transition e_W1 := {
		from := q;
		to := q;
		guard := pc1 = 2 && a <= s;
		action := pc1' = 3;
	};

	transition e_C1 := {
		from := q;
		to := q;
		guard := pc1 = 3;
		action := pc1' = 1, s' = s + 1;
	};

	transition e_T2 := {
		from := q;
		to := q;
		guard := pc2 = 1;
		action := pc2' = 2, b' = t, t' = t + 1;
	};

	transition e_W2 := {
		from := q;
		to := q;
		guard := pc2 = 2 && b <= s;
		action := pc2' = 3;
	};

	transition e_C2 := {
		from := q;
		to := q;
		guard := pc2 = 3;
		action := pc2' = 1, s' = s + 1;
	};

}

strategy s {

	Region init := {state = q && t = s && pc1 = 1 && pc2 = 1};

	Region bad := {pc1 = 3 && pc2 = 3};

}

