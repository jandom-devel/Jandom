//! @tags fixpoint
//! @citations Gonnord_thesis:f7.6:p96

model gonnord_thesis_096b {

	var x, y, z;
	states q;

	transition t1 := {
		from := q;
		to := q;
		guard := x >= 2z;
		action := x' = x + 1, y ' = y + 1, z' = z + 1;
	};

	transition t2 := {
		from := q;
		to := q;
		guard := true;
		action := z' = 0;
	};

}

strategy s {

	Region init := {state = q && x = 0 && y = 0 && z = 0};

	Region bad := {x < 0 || y < 0 || z < 0 || z > x || x > y};

}

