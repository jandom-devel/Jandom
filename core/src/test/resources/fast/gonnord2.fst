//! @tags fixpoint
//! @citations Gonnord_thesis:f4.7:p55

model gonnord_thesis_055 {

	var i, j, k;
	states k1, k2;

	transition t1 := {
		from := k1;
		to := k1;
		guard := i <= 100 && j < 9;
		action := i' = i + 2, k' = k + 1, j' = j + 1;
	};

	transition t2 := {
		from := k1;
		to := k1;
		guard := i <= 100 && j < 9;
		action := i' = i + 2, j' = j + 1;
	};

	transition t3 := {
		from := k1;
		to := k1;
		guard := i <= 100 && j = 9;
		action := i' = i + 2, k' = k + 2, j' = 0;
	};

	transition t4 := {
		from := k1;
		to := k2;
		guard := i > 100;
		action := ;
	};

}

strategy s {

	Region init := {state = k1 && i = 0 && j = 0 && k = 0};

	Region bad := {i < 2j || 11i < 2j + 20k || i > 2j + 10k || j < 0};

}

