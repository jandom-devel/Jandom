//! @tags complexity
//! @citations CookPR06:f3:p4

model cook_podelski_rybalchenko_pldi06_04 {

	var x0, z0, x, z, n, c1, c2;
	states q1, q2;

	transition t1 := {
		from := q1;
		to := q2;
		guard := x < n;
		action := ;
	};

	transition t2 := {
		from := q2;
		to := q1;
		guard := z > x;
		action := x' = x + 1, c1' = c1 + 1;
	};

	transition t3 := {
		from := q2;
		to := q1;
		guard := z <= x;
		action := z' = z + 1, c2' = c2 + 1;
	};

}

strategy s {

	Region init := {state = q1 && x = x0 && z = z0 && c1 = 0 && c2 = 0 && n >= x0 && n >= z0};

	Region bad := {c1 + c2 > 2n - x0 - z0};

}

