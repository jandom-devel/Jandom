//! @tags complexity
//! @citations GulwaniJK09:f1:p2

model gulwani_jain_koskinen_pldi09_02 {

	var id, tmp, max_id, i;
	states q;

	transition t1 := {
		from := q;
		to := q;
		guard := tmp != id && tmp <= max_id;
		action := tmp' = tmp + 1, i' = i + 1;
	};

	transition t2 := {
		from := q;
		to := q;
		guard := tmp != id && tmp > max_id;
		action := tmp' = 0, i' = i + 1;
	};

}

strategy s {

	Region init := {state = q && 0 <= id && id < max_id && tmp = id + 1 && i = 0};

	Region bad := {i > max_id + 1};

}

