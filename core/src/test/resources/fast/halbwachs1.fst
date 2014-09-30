//! @tags fixpoint
//! @citations Gonnord_thesis:f2.12:p31 Halbwachs_thesis

model gonnord_thesis_031 {

	var i, j;
	states k1, k2;

	transition t1 := {
		from := k1;
		to := k1;
		guard := i <= 100;
		action := i' = i + 4;
	};

	transition t2 := {
		from := k1;
		to := k1;
		guard := i <= 100;
		action := i' = i + 2, j' = j + 1;
	};

	transition t3 := {
		from := k1;
		to := k2;
		guard := i > 100;
		action := ;
	};

}

strategy s {

	Region init := {state = k1 && i = 0 && j = 0};

	Region bad := {2j > i || 0 > j};

}

