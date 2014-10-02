//! @tags fixpoint
//! @citations Gonnord_thesis:f7.4:p93

model gonnord_thesis_93b {

	var x, z;
	states q;

	transition t1 := {
		from := q;
		to := q;
		guard := x >= 2z;
		action := x' = x + 1, z' = z + 1;
	};

	transition t2 := {
		from := q;
		to := q;
		guard := true;
		action := z' = 0;
	};

}

strategy s {

	Region init := {state = q && x = 0 && z = 0};

	Region bad := {x < z || z < 0 || x < 2z - 1};

}

