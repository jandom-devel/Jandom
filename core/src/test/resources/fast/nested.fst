//! @tags complexity nonlinear
//! @citations GulwaniJK09:f2:p3

model gulwani_jain_koskinen_pldi09_03 {

	var n, m, N, i, j, k, c;
	states L1, L2, L3;

	transition t1 := {
		from := L1;
		to := L2;
		guard := i < n;
		action := j' = 0, c' = c + 1;
	};

	transition t2 := {
		from := L2;
		to := L3;
		guard := j < m;
		action := j' = j + 1, k' = i, c' = c + 1;
	};

	transition t3 := {
		from := L3;
		to := L3;
		guard := k < N;
		action := k' = k + 1, c' = c + 1;
	};

	transition t4 := {
		from := L3;
		to := L2;
		guard := true;
		action := i' = k;
	};

	transition t5 := {
		from := L2;
		to := L1;
		guard := true;
		action := i' = i + 1;
	};

}

strategy s {

	Region init := {state = L1 && 0 <= n && 0 <= m && 0 <= N};

	Region bad := {c > n + m * n + N};

}

