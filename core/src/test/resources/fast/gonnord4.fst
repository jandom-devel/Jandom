//! @tags fixpoint
//! @citations Gonnord_thesis:f7.3:p93

model gonnord_thesis_093a {

	var x, z;
	states q;

	transition t1 := {
		from := q;
		to := q;
		guard := z <= 9;
		action := x' = x + 1, z' = z + 1;
	};

	transition t2 := {
		from := q;
		to := q;
		guard := z = 10;
		action := z' = 0;
	};

}

strategy s {

	Region init := {state = q && x = 0 && z = 0};

	Region bad := {z > x || z > 10 || z < 0};

}

