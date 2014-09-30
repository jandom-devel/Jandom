//! @tags termination
//! @citations GulavaniHKNR06:p3:f1

model gulavani_henzinger_kannan_nori_rajamani_sigsoft06_03 {

	var i, c;
	states q;

	transition t := {
		from := q;
		to := q;
		guard := i < 1000;
		action := c' = c + i, i' = i + 1;
	};

}

strategy s {
	
	Region init := {state = q && i = 0 && c = 0};

}

