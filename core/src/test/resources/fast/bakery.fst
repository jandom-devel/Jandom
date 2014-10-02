//! @citations BultanGP97:f1:p3
//! @tags fixpoint parallelism

model bultan_gerber_pugh_cav97_03 {

	var a, b, pc1, pc2;
	states q;

	transition e_T1 := {
		from := q;
		to := q;
		guard := pc1 = 1;
		action := pc1' = 2, a' = b + 1;
	};

	transition e_W1a := {
		from := q;
		to := q;
		guard := pc1 = 2 && a < b;
		action := pc1' = 3;
	};

	transition e_W1b := {
		from := q;
		to := q;
		guard := pc1 = 2 && b = 0;
		action := pc1' = 3;
	};

	transition e_C1 := {
		from := q;
		to := q;
		guard := pc1 = 3;
		action := pc1' = 1, a' = 0;
	};

	transition e_T2 := {
		from := q;
		to := q;
		guard := pc2 = 1;
		action := pc2' = 2, b' = a + 2;
	};

	transition e_W2a := {
		from := q;
		to := q;
		guard := pc2 = 2 && b < a;
		action := pc2' = 3;
	};

	transition e_W2b := {
		from := q;
		to := q;
		guard := pc2 = 2 && a = 0;
		action := pc2' = 3;
	};

	transition e_C2 := {
		from := q;
		to := q;
		guard := pc2 = 3;
		action := pc2' = 1, b' = 0;
	};

}

strategy s {

	Region init := {state = q && a = 0 && b = 0 && pc1 = 1 && pc2 = 1};

	Region bad := {pc1 = 3 && pc2 = 3};

}

