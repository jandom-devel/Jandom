// Example 'hybrid' in the paper "G. Amato, F. Scozzari. Localizing widening and narrowing. SAS 2013"
// This example show the improvement it is possible to obtain through the use of localized narrowing
// with restart policy.

model amato_scozzari_sas13_hybrid {

	var i, j;	
	states s1, s2, s3;
	
	transition t1 := {
		from := s1;
		to := s2;
		guard := true;
		action := j' = 0, i' = i + 1;
	};
	
	transition t2 := {
		from := s2;
		to := s2;
		guard := j < 10;
		action := j' = j + 1;
	};
	
	transition t3 := {
		from := s2;
		to := s1;
		guard := j >= 10 && i > 9;
		action := i'=0;
	};
	
	transition t4 := {
		from := s2;
		to := s1;
		guard := j >= 10 && i <= 9;
		action := ;
	};	
}

strategy s {
	Region init := { state = s1 && i = 0 };
}
